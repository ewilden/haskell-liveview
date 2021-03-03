"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.instrumentHsaction = void 0;
function instrumentHsaction(root, callback) {
    let cleanupCallbacks = Array.from(root.querySelectorAll('[hsaction]'))
        .flatMap(instrumentNode(callback));
    return () => {
        cleanupCallbacks.forEach(cleanup => void cleanup());
        cleanupCallbacks = [];
    };
}
exports.instrumentHsaction = instrumentHsaction;
function parseDebounce(attr) {
    return attr === "blur" ? "blur" : +attr;
}
function listenOnce(node, event) {
    const prom = new ResolvablePromise();
    const listener = (e) => {
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
function wait(timeMs) {
    const prom = new ResolvablePromise();
    const timeoutId = setTimeout(prom.resolve, timeMs);
    prom.promise.catch(() => void clearTimeout(timeoutId));
    return prom;
}
function throttleFn(f, timeMs) {
    let timer;
    return (t) => {
        (async () => {
            if (timer) {
                await timer;
            }
            f(t);
            timer = wait(timeMs).promise;
        })();
    };
}
class ResolvablePromise {
    constructor() {
        this.promise = new Promise((resolve, reject) => {
            this.resolve = resolve;
            this.reject = reject;
        });
    }
    cancel(e) {
        this.reject(e || "cancelled");
    }
}
function listenDebounced(node, event, debounce, callback) {
    let cleanupPendings = [];
    const listener = (e) => {
        (async () => {
            const waitPromise = debounce === "blur" ? listenOnce(node, 'blur') : wait(debounce);
            cleanupPendings.forEach(f => f());
            cleanupPendings = [];
            cleanupPendings.push(() => waitPromise.cancel());
            await waitPromise.promise;
            callback(e);
        })();
    };
    node.addEventListener(event, listener);
    return [() => node.removeEventListener(event, listener), () => cleanupPendings.forEach(f => f())];
}
const instrumentNode = (callback) => (node) => {
    const eventActionPairs = node.getAttribute('hsaction').split(';').map(s => s.split(':'));
    const debounce = node.hasAttribute("hsdebounce") ?
        parseDebounce(node.getAttribute("hsdebounce")) : undefined;
    const throttle = node.hasAttribute('hsthrottle') ? +node.getAttribute('hsthrottle') : undefined;
    const cleanupCallbacks = [];
    for (const [event, action] of eventActionPairs) {
        let listener = (e) => {
            const payload = {};
            payload.value = String(node.value);
            let key = null;
            for (const attrName of node.getAttributeNames()) {
                if (attrName.startsWith("hsvalue-")) {
                    payload[attrName.slice("hsvalue-".length)] = String(node.getAttribute(attrName));
                }
                else if (attrName === "hskey") {
                    key = node.getAttribute("hskey");
                }
            }
            if (event === 'keyup' || event === 'keydown') {
                if (e.key !== key) {
                    return;
                }
            }
            callback({ action, payload });
        };
        if (debounce) {
            cleanupCallbacks.push(...listenDebounced(node, event, debounce, listener));
        }
        else {
            if (throttle) {
                listener = throttleFn(listener, throttle);
            }
            node.addEventListener(event, listener);
            cleanupCallbacks.push(() => node.removeEventListener(event, listener));
        }
    }
    return cleanupCallbacks;
};
