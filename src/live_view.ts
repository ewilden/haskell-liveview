import {CallConsumer, CleanupCallback, ResolvablePromise, instrumentHsaction} from './hsaction';
import morphdom from 'morphdom';
import {Monad, Functor, _} from 'hkts/src';
import { MulticastDisposable } from '@most/core/dist/combinator/multicast';
import { Repeater, Push, Stop } from "@repeaterjs/repeater";

// const WebSocketM: Functor<WebSocketApp<_>> = {
//   // of: a => new WebSocketApp(a),
//   map: (f, m) => new WebSocketApp(f(m.x)),
// };

// class WebSocketApp<A> {
//   constructor(readonly x: A) {}
// }

interface WSMessage {
  type: "message";
  event: MessageEvent;
}
interface WSError {
  type: "error";
  event: Event;
}
interface WSClose {
  type: "close";
  event: CloseEvent;
}
interface WSOpen {
  type: "open";
  event: Event;
}
interface WSWrite {
  type: "write";
  data: WebSocketWriteData;
}

type WebSocketWriteData = string | ArrayBuffer | SharedArrayBuffer | Blob | ArrayBufferView;

interface AsyncIteratorWithSender<T, R=any> extends AsyncGenerator<T, R> {
  send(t: T): Promise<void>;
  close(r: R): Promise<void>;
}

type InnerMsg<T, R> = {type: "send", send: T}|{type: "close", close: R};

// function mkResolvableAsyncIterator<T, R=any>(): AsyncIteratorWithSender<T, R> {
//   let push;
//   let stop;
//   const queue: Repeater<T, R> = new Repeater(async (push_, stop_) => {
//     push = push_;
//     stop = stop_;
//   });
//   return {
//     send: t => push({type: "send", send: t}),
//     close: r => stop({type: "close", close: r}),
//     ...queue,
//   };
  // let currRP: ResolvablePromise<InnerMsg<T, R>> 
  //   = new ResolvablePromise();
  // let semaphore: ResolvablePromise<0> = new ResolvablePromise();
  // semaphore.resolve(0);
  // async function* gen() {
  //   while (true) {
  //     const msg = await currRP.promise;
  //     if (msg.type === "send") {
  //       yield msg.send;
  //     } else {
  //       return msg.close;
  //     }
  //   }
  // }
  // async function dispatch(msg: InnerMsg<T, R>) {
  //   await semaphore.promise;
  //   semaphore = new ResolvablePromise();
  //   currRP.resolve(msg);
  //   currRP = new ResolvablePromise();
  // }
  // return {
  //   ...gen(),
  //   send: t => dispatch({type: "send", send: t}),
  //   close: r => dispatch({type: "close", close: r}),
  // }
// }

type WSEvent = WSMessage | WSError | WSClose | WSOpen;

interface WebSocketHandle {
  messages: AsyncIterator<WSMessage, WSError | WSClose>;
  send: (data: WebSocketWriteData) => void;
  close: (code?: number | undefined, reason?: string | undefined) => void;
  [Symbol.asyncIterator](): AsyncIterator<WSMessage, WSError | WSClose>;
}

function mkHandle(url: string): WebSocketHandle {
  const webSocket = new WebSocket(url);
  const messages: Repeater<WSMessage, WSError | WSClose> = new Repeater(
    async (push: Push<WSMessage>, stop: Stop) => {
      return new Promise(resolve => {
        webSocket.addEventListener("close", event => resolve({type: "close", event}));
        webSocket.addEventListener("error", event => resolve({type: "error", event}));
        webSocket.addEventListener("message", event => push({type: "message", event}));
      });
    });
  const send = webSocket.send.bind(webSocket);
  const close = webSocket.close.bind(webSocket);
  return {
    messages, 
    send, 
    close, 
    [Symbol.asyncIterator]: () => messages
  };
}

class IterableWebSocket {
  private readonly webSocketHandle: WebSocketHandle;
  readonly messages: AsyncIterator<WSMessage, WSError | WSClose>;

  constructor(url: string) {
    this.webSocketHandle = mkHandle(url);
    this.messages = this.webSocketHandle.messages;
  }

  send(data: WebSocketWriteData) {
    this.webSocketHandle.send(data);
  }

  close(code?: number | undefined, reason?: string | undefined) {
    this.webSocketHandle.close(code, reason);
  }

  [Symbol.asyncIterator]() {
    return this.webSocketHandle.messages;
  }
}

interface WebSocketState {
  socket: WebSocket;
  clock: number;
}

type PatchEntry = ["keep"] | ["delete"] | ["insert", string];
type LiveViewMountMessage = ["mount", string[]];
type LiveViewPatchMessage = ["patch", PatchEntry[]];

type LiveViewMessage = [LiveViewMountMessage | LiveViewPatchMessage, number];

function parseWSMessage(wsMessage: WSMessage): LiveViewMessage {
  // TODO: validate
  return JSON.parse(wsMessage.event.data);
}

function applyPatch(currArray: string[], patches: PatchEntry[]): string[] {
  return patches.flatMap((patchEntry, i) => {
    if (patchEntry[0] === "delete") {
      return [];
    } else if (patchEntry[0] === "keep") {
      return [currArray[i]];
    } else {
      return [patchEntry[1]];
    }
  });
}

export async function attach(root: Element, wsUrl: string): Promise<WSClose> {
  const ws = new IterableWebSocket(wsUrl);
  let currClock = 0;
  let currArray: string[]|undefined;
  const cleanup = instrumentHsaction(root, call => {
    ws.send(JSON.stringify([call, currClock]));
  });
  try {
    return await (async () => {
      while (true) {
        const nxt = await ws.messages.next();
        if (nxt.done) {
          if (nxt.value.type === "close") {
            return nxt.value;
          } else {
            throw nxt.value;
          }
        } else {
          const rawMessage = nxt.value;
          const [msg, clock] = parseWSMessage(rawMessage);
          currClock = clock;
          if (msg[0] === 'mount') {
            currArray = msg[1];
          } else {
            if (!currArray) {
              throw new Error("Tried to mount with no currArray.");
            }
            currArray = applyPatch(currArray, msg[1]);
          }
          morphdom(root, currArray.join(''));
        }
      }
    })();
  } finally {
    cleanup();
  }
}