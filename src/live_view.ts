import {instrumentHsaction} from './hsaction';
import morphdom from 'morphdom';
import { Repeater, Push, Stop } from "@repeaterjs/repeater";

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

type WSEvent = WSMessage | WSError | WSClose | WSOpen;

interface WebSocketHandle {
  messages: Repeater<WSMessage, WSError | WSClose>;
  send: (data: WebSocketWriteData) => void;
  close: (code?: number | undefined, reason?: string | undefined) => void;
  [Symbol.asyncIterator](): Repeater<WSMessage, WSError | WSClose>;
}

function mkHandle(url: string): WebSocketHandle {
  const webSocket = new WebSocket(url);
  const messages: Repeater<WSMessage, WSError | WSClose> = new Repeater(
    async (push: Push<WSMessage>, stop: Stop) => {
      return new Promise(resolve => {
        webSocket.addEventListener("close", event => resolve({type: "close", event}));
        webSocket.addEventListener("error", event => resolve({type: "error", event}));
        webSocket.addEventListener("message", event => {
          push({type: "message", event});
          console.log(event);
        });
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

type PatchEntry = ["keep"] | ["delete"] | ["insert", string];
type LiveViewMountMessage = ["mount", string[]];
type LiveViewPatchMessage = ["patch", PatchEntry[]];

type LiveViewMessage = [LiveViewMountMessage | LiveViewPatchMessage, number];

function parseWSMessage(wsMessage: WSMessage): LiveViewMessage {
  // TODO: validate
  console.log('parsing');
  const parsed = JSON.parse(wsMessage.event.data);
  console.log(parsed);
  return parsed;
}

function applyPatch(currArray: string[], patches: PatchEntry[]): string[] {
  const out = [];
  let currIndex = 0;
  for (const patchEntry of patches) {
      if (patchEntry[0] === "delete") {
        currIndex++;
      } else if (patchEntry[0] === "keep") {
        out.push(currArray[currIndex++]);
      } else {
        out.push(patchEntry[1]);
      }
  }
  if (currIndex !== currArray.length) {
    throw new Error(`Assertion failed: ${currIndex} !== ${currArray.length}`)
  }
  return out;
}

export async function attach(root: Element, wsUrl: string): Promise<WSClose> {
  const ws = mkHandle(wsUrl);
  let currClock = 0;
  let currArray: string[]|undefined;
  let cleanup = instrumentHsaction(root, call => {
    console.log('sending');
    console.log(call);
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
          const toMorph = `${currArray.join('')}`;
          console.log(root);
          console.log('morphing');
          console.log(toMorph);
          console.log(root.innerHTML);
          cleanup();
          // morphdom(root, toMorph);
          // console.log(root.innerHTML);
          // root.innerHTML = toMorph;
          cleanup = instrumentHsaction(root, call => {
            console.log('sending');
            console.log(call);
            ws.send(JSON.stringify([call, currClock]));
          });
        }
      }
    })();
  } finally {
    cleanup();
  }
}