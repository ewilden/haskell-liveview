/**
 * Copyright 2021 Google LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

export interface ActionCall {
  action: string;
  payload: Record<string, string>;
}

export type CallConsumer = (call: ActionCall) => void;
export type CleanupCallback = () => void;

export function instrumentHsaction(root: Element, callback: CallConsumer): CleanupCallback {
  let cleanupCallbacks: CleanupCallback[] = Array.from(root.querySelectorAll('[hsaction]'))
    .flatMap(instrumentNode(callback));
  return () => {
    cleanupCallbacks.forEach(cleanup => cleanup());
    cleanupCallbacks = [];
  };
}

type DebounceSpec = "blur"|number;

function parseDebounce(attr: string): DebounceSpec {
  return attr === "blur" ? "blur" : +attr;
}

interface CancelablePromise<T> {
  promise: Promise<T>;
  cancel: () => void;
}

function listenOnce(node: Element, event: string): CancelablePromise<Event> {
  const prom = new ResolvablePromise<Event>();
  const listener = (e: Event) => {
    prom.resolve(e);
    cleanup();
  };
  const cleanup = () => {
    node.removeEventListener(event, listener);
  };
  node.addEventListener(event, listener);
  prom.promise.catch(() => {
    cleanup();
  });
  return prom;
}

function wait(timeMs: number): CancelablePromise<void> {
  const prom = new ResolvablePromise<void>();
  const timeoutId = setTimeout(prom.resolve, timeMs);
  prom.promise.catch(() => void clearTimeout(timeoutId));
  return prom;
}

function throttleFn<T>(f: (t: T) => void, timeMs: number): (t: T) => void {
  let lastCall = Date.now();
  return (t: T) => {
    const now = Date.now();
    if (now - lastCall < timeMs) {
      return;
    }
    lastCall = now;
    f(t);
  };
}

export class ResolvablePromise<T> implements CancelablePromise<T> {
  readonly promise: Promise<T>;
  resolve!: (t: T) => void;
  reject!: (e: any) => void;

  constructor() {
    this.promise = new Promise((resolve, reject) => {
      this.resolve = resolve;
      this.reject = reject;
    });
  }

  cancel(e?: any) {
    this.reject(e || "cancelled");
  }
}

function listenDebounced(node: Element, 
    event: string, debounce: DebounceSpec, callback: (e: Event) => void): CleanupCallback[] {

  let cleanupPendings: CleanupCallback[] = [];
  const listener = (e: Event) => {
    (async () => {
      const waitPromise: CancelablePromise<unknown> = debounce === "blur" ? listenOnce(node, 'blur') : wait(debounce);
      cleanupPendings.forEach(f => f());
      cleanupPendings = [];
      cleanupPendings.push(() => waitPromise.cancel());
      try {
        await waitPromise.promise;
        callback(e);
      } catch (err) {
        // pass
      }
    })();
  };
  node.addEventListener(event, listener);
  return [() => node.removeEventListener(event, listener), () => cleanupPendings.forEach(f => f())];
}

const instrumentNode: (callback: CallConsumer) => (node: Element) => CleanupCallback[] = 
    (callback: CallConsumer) => (node: Element) => {
  const eventActionPairs = node.getAttribute('hsaction')!.split(';').map(s => s.split(':').map(st => st.trim()) as [string, string]);
  const actionsToPreventDefault = new Set<string>((node.getAttribute('hsprevent') ?? "").split(';'));
 
  // console.log(eventActionPairs);
  const debounce: DebounceSpec|undefined = node.hasAttribute("hsdebounce") ? 
    parseDebounce(node.getAttribute("hsdebounce")!) : undefined;
  const throttle: number|undefined = node.hasAttribute('hsthrottle') ? +node.getAttribute('hsthrottle')! : undefined;

  const cleanupCallbacks: CleanupCallback[] = [];
  for (const [event, action] of eventActionPairs) {
    let listener = (e: Event) => {
      const payload = {} as Record<string, string>;
      const mayValue = (node as HTMLInputElement).value;
      if (mayValue != null) {
        payload.value = String(mayValue);
      }
      let key: string|null = null;
      for (const attrName of node.getAttributeNames()) {
        if (attrName.startsWith("hsvalue-")) {
          payload[attrName.slice("hsvalue-".length)] = String(node.getAttribute(attrName));
        } else if (attrName === "hskey") {
          key = node.getAttribute("hskey");
        }
      }
      if (event === 'keyup' || event === 'keydown') {
        if (key && ((e as KeyboardEvent).key !== key)) {
          return;
        }
      }
      callback({action, payload});
      if (actionsToPreventDefault.has(event)) {
        e.preventDefault();
      }
    };
    if (debounce) {
      cleanupCallbacks.push(...listenDebounced(node, event, debounce, listener));
    } else {
      if (throttle) {
        listener = throttleFn(listener, throttle);
      }
      // console.log(`adding listener ${listener} to ${node} for ${event}`)
      node.addEventListener(event, listener);
      cleanupCallbacks.push(() => node.removeEventListener(event, listener));
    }
  }
  return cleanupCallbacks;
}

