(() => {
  // src/hsaction.ts
  function instrumentHsaction(root, callback) {
    let cleanupCallbacks = Array.from(root.querySelectorAll("[hsaction]")).flatMap(instrumentNode(callback));
    return () => {
      cleanupCallbacks.forEach((cleanup) => cleanup());
      cleanupCallbacks = [];
    };
  }
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
    let lastCall = Date.now();
    return (t) => {
      const now = Date.now();
      if (now - lastCall < timeMs) {
        return;
      }
      lastCall = now;
      f(t);
    };
  }
  var ResolvablePromise = class {
    constructor() {
      this.promise = new Promise((resolve, reject2) => {
        this.resolve = resolve;
        this.reject = reject2;
      });
    }
    cancel(e) {
      this.reject(e || "cancelled");
    }
  };
  function listenDebounced(node, event, debounce, callback) {
    let cleanupPendings = [];
    const listener = (e) => {
      (async () => {
        const waitPromise = debounce === "blur" ? listenOnce(node, "blur") : wait(debounce);
        cleanupPendings.forEach((f) => f());
        cleanupPendings = [];
        cleanupPendings.push(() => waitPromise.cancel());
        try {
          await waitPromise.promise;
          callback(e);
        } catch (err) {
        }
      })();
    };
    node.addEventListener(event, listener);
    return [() => node.removeEventListener(event, listener), () => cleanupPendings.forEach((f) => f())];
  }
  var instrumentNode = (callback) => (node) => {
    const eventActionPairs = node.getAttribute("hsaction").split(";").map((s) => s.split(":").map((st) => st.trim()));
    const debounce = node.hasAttribute("hsdebounce") ? parseDebounce(node.getAttribute("hsdebounce")) : void 0;
    const throttle = node.hasAttribute("hsthrottle") ? +node.getAttribute("hsthrottle") : void 0;
    const cleanupCallbacks = [];
    for (const [event, action] of eventActionPairs) {
      let listener = (e) => {
        const payload = {};
        const mayValue = node.value;
        if (mayValue) {
          payload.value = String(mayValue);
        }
        let key = null;
        for (const attrName of node.getAttributeNames()) {
          if (attrName.startsWith("hsvalue-")) {
            payload[attrName.slice("hsvalue-".length)] = String(node.getAttribute(attrName));
          } else if (attrName === "hskey") {
            key = node.getAttribute("hskey");
          }
        }
        if (event === "keyup" || event === "keydown") {
          if (e.key !== key) {
            return;
          }
        }
        callback({action, payload});
      };
      if (debounce) {
        cleanupCallbacks.push(...listenDebounced(node, event, debounce, listener));
      } else {
        if (throttle) {
          listener = throttleFn(listener, throttle);
        }
        node.addEventListener(event, listener);
        cleanupCallbacks.push(() => node.removeEventListener(event, listener));
      }
    }
    return cleanupCallbacks;
  };

  // node_modules/@repeaterjs/repeater/repeater.js
  /*! *****************************************************************************
  Copyright (c) Microsoft Corporation.
  
  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted.
  
  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
  REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
  AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
  INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
  OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
  PERFORMANCE OF THIS SOFTWARE.
  ***************************************************************************** */
  var extendStatics = function(d, b) {
    extendStatics = Object.setPrototypeOf || {__proto__: []} instanceof Array && function(d2, b2) {
      d2.__proto__ = b2;
    } || function(d2, b2) {
      for (var p in b2)
        if (b2.hasOwnProperty(p))
          d2[p] = b2[p];
    };
    return extendStatics(d, b);
  };
  function __extends(d, b) {
    extendStatics(d, b);
    function __() {
      this.constructor = d;
    }
    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
  }
  function __awaiter(thisArg, _arguments, P, generator) {
    function adopt(value) {
      return value instanceof P ? value : new P(function(resolve) {
        resolve(value);
      });
    }
    return new (P || (P = Promise))(function(resolve, reject2) {
      function fulfilled(value) {
        try {
          step(generator.next(value));
        } catch (e) {
          reject2(e);
        }
      }
      function rejected(value) {
        try {
          step(generator["throw"](value));
        } catch (e) {
          reject2(e);
        }
      }
      function step(result) {
        result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected);
      }
      step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
  }
  function __generator(thisArg, body) {
    var _ = {label: 0, sent: function() {
      if (t[0] & 1)
        throw t[1];
      return t[1];
    }, trys: [], ops: []}, f, y, t, g;
    return g = {next: verb(0), throw: verb(1), return: verb(2)}, typeof Symbol === "function" && (g[Symbol.iterator] = function() {
      return this;
    }), g;
    function verb(n) {
      return function(v) {
        return step([n, v]);
      };
    }
    function step(op) {
      if (f)
        throw new TypeError("Generator is already executing.");
      while (_)
        try {
          if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done)
            return t;
          if (y = 0, t)
            op = [op[0] & 2, t.value];
          switch (op[0]) {
            case 0:
            case 1:
              t = op;
              break;
            case 4:
              _.label++;
              return {value: op[1], done: false};
            case 5:
              _.label++;
              y = op[1];
              op = [0];
              continue;
            case 7:
              op = _.ops.pop();
              _.trys.pop();
              continue;
            default:
              if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) {
                _ = 0;
                continue;
              }
              if (op[0] === 3 && (!t || op[1] > t[0] && op[1] < t[3])) {
                _.label = op[1];
                break;
              }
              if (op[0] === 6 && _.label < t[1]) {
                _.label = t[1];
                t = op;
                break;
              }
              if (t && _.label < t[2]) {
                _.label = t[2];
                _.ops.push(op);
                break;
              }
              if (t[2])
                _.ops.pop();
              _.trys.pop();
              continue;
          }
          op = body.call(thisArg, _);
        } catch (e) {
          op = [6, e];
          y = 0;
        } finally {
          f = t = 0;
        }
      if (op[0] & 5)
        throw op[1];
      return {value: op[0] ? op[1] : void 0, done: true};
    }
  }
  function __values(o) {
    var s = typeof Symbol === "function" && Symbol.iterator, m = s && o[s], i = 0;
    if (m)
      return m.call(o);
    if (o && typeof o.length === "number")
      return {
        next: function() {
          if (o && i >= o.length)
            o = void 0;
          return {value: o && o[i++], done: !o};
        }
      };
    throw new TypeError(s ? "Object is not iterable." : "Symbol.iterator is not defined.");
  }
  function __await(v) {
    return this instanceof __await ? (this.v = v, this) : new __await(v);
  }
  function __asyncGenerator(thisArg, _arguments, generator) {
    if (!Symbol.asyncIterator)
      throw new TypeError("Symbol.asyncIterator is not defined.");
    var g = generator.apply(thisArg, _arguments || []), i, q = [];
    return i = {}, verb("next"), verb("throw"), verb("return"), i[Symbol.asyncIterator] = function() {
      return this;
    }, i;
    function verb(n) {
      if (g[n])
        i[n] = function(v) {
          return new Promise(function(a, b) {
            q.push([n, v, a, b]) > 1 || resume(n, v);
          });
        };
    }
    function resume(n, v) {
      try {
        step(g[n](v));
      } catch (e) {
        settle(q[0][3], e);
      }
    }
    function step(r) {
      r.value instanceof __await ? Promise.resolve(r.value.v).then(fulfill, reject2) : settle(q[0][2], r);
    }
    function fulfill(value) {
      resume("next", value);
    }
    function reject2(value) {
      resume("throw", value);
    }
    function settle(f, v) {
      if (f(v), q.shift(), q.length)
        resume(q[0][0], q[0][1]);
    }
  }
  var RepeaterOverflowError = function(_super) {
    __extends(RepeaterOverflowError2, _super);
    function RepeaterOverflowError2(message) {
      var _this = _super.call(this, message) || this;
      Object.defineProperty(_this, "name", {
        value: "RepeaterOverflowError",
        enumerable: false
      });
      if (typeof Object.setPrototypeOf === "function") {
        Object.setPrototypeOf(_this, _this.constructor.prototype);
      } else {
        _this.__proto__ = _this.constructor.prototype;
      }
      if (typeof Error.captureStackTrace === "function") {
        Error.captureStackTrace(_this, _this.constructor);
      }
      return _this;
    }
    return RepeaterOverflowError2;
  }(Error);
  var FixedBuffer = function() {
    function FixedBuffer2(capacity) {
      if (capacity < 0) {
        throw new RangeError("Capacity may not be less than 0");
      }
      this._c = capacity;
      this._q = [];
    }
    Object.defineProperty(FixedBuffer2.prototype, "empty", {
      get: function() {
        return this._q.length === 0;
      },
      enumerable: false,
      configurable: true
    });
    Object.defineProperty(FixedBuffer2.prototype, "full", {
      get: function() {
        return this._q.length >= this._c;
      },
      enumerable: false,
      configurable: true
    });
    FixedBuffer2.prototype.add = function(value) {
      if (this.full) {
        throw new Error("Buffer full");
      } else {
        this._q.push(value);
      }
    };
    FixedBuffer2.prototype.remove = function() {
      if (this.empty) {
        throw new Error("Buffer empty");
      }
      return this._q.shift();
    };
    return FixedBuffer2;
  }();
  var SlidingBuffer = function() {
    function SlidingBuffer2(capacity) {
      if (capacity < 1) {
        throw new RangeError("Capacity may not be less than 1");
      }
      this._c = capacity;
      this._q = [];
    }
    Object.defineProperty(SlidingBuffer2.prototype, "empty", {
      get: function() {
        return this._q.length === 0;
      },
      enumerable: false,
      configurable: true
    });
    Object.defineProperty(SlidingBuffer2.prototype, "full", {
      get: function() {
        return false;
      },
      enumerable: false,
      configurable: true
    });
    SlidingBuffer2.prototype.add = function(value) {
      while (this._q.length >= this._c) {
        this._q.shift();
      }
      this._q.push(value);
    };
    SlidingBuffer2.prototype.remove = function() {
      if (this.empty) {
        throw new Error("Buffer empty");
      }
      return this._q.shift();
    };
    return SlidingBuffer2;
  }();
  var DroppingBuffer = function() {
    function DroppingBuffer2(capacity) {
      if (capacity < 1) {
        throw new RangeError("Capacity may not be less than 1");
      }
      this._c = capacity;
      this._q = [];
    }
    Object.defineProperty(DroppingBuffer2.prototype, "empty", {
      get: function() {
        return this._q.length === 0;
      },
      enumerable: false,
      configurable: true
    });
    Object.defineProperty(DroppingBuffer2.prototype, "full", {
      get: function() {
        return false;
      },
      enumerable: false,
      configurable: true
    });
    DroppingBuffer2.prototype.add = function(value) {
      if (this._q.length < this._c) {
        this._q.push(value);
      }
    };
    DroppingBuffer2.prototype.remove = function() {
      if (this.empty) {
        throw new Error("Buffer empty");
      }
      return this._q.shift();
    };
    return DroppingBuffer2;
  }();
  function swallow(value) {
    if (value != null && typeof value.then === "function") {
      value.then(NOOP, NOOP);
    }
  }
  var Initial = 0;
  var Started = 1;
  var Stopped = 2;
  var Done = 3;
  var Rejected = 4;
  var MAX_QUEUE_LENGTH = 1024;
  var NOOP = function() {
  };
  function consumeExecution(r) {
    var err = r.err;
    var execution = Promise.resolve(r.execution).then(function(value) {
      if (err != null) {
        throw err;
      }
      return value;
    });
    r.err = void 0;
    r.execution = execution.then(function() {
      return void 0;
    }, function() {
      return void 0;
    });
    return r.pending === void 0 ? execution : r.pending.then(function() {
      return execution;
    });
  }
  function createIteration(r, value) {
    var done = r.state >= Done;
    return Promise.resolve(value).then(function(value2) {
      if (!done && r.state >= Rejected) {
        return consumeExecution(r).then(function(value3) {
          return {
            value: value3,
            done: true
          };
        });
      }
      return {value: value2, done};
    });
  }
  function stop(r, err) {
    var e_1, _a;
    if (r.state >= Stopped) {
      return;
    }
    r.state = Stopped;
    r.onnext();
    r.onstop();
    if (r.err == null) {
      r.err = err;
    }
    if (r.pushes.length === 0 && (typeof r.buffer === "undefined" || r.buffer.empty)) {
      finish(r);
    } else {
      try {
        for (var _b = __values(r.pushes), _d = _b.next(); !_d.done; _d = _b.next()) {
          var push_1 = _d.value;
          push_1.resolve();
        }
      } catch (e_1_1) {
        e_1 = {error: e_1_1};
      } finally {
        try {
          if (_d && !_d.done && (_a = _b.return))
            _a.call(_b);
        } finally {
          if (e_1)
            throw e_1.error;
        }
      }
    }
  }
  function finish(r) {
    var e_2, _a;
    if (r.state >= Done) {
      return;
    }
    if (r.state < Stopped) {
      stop(r);
    }
    r.state = Done;
    r.buffer = void 0;
    try {
      for (var _b = __values(r.nexts), _d = _b.next(); !_d.done; _d = _b.next()) {
        var next = _d.value;
        var execution = r.pending === void 0 ? consumeExecution(r) : r.pending.then(function() {
          return consumeExecution(r);
        });
        next.resolve(createIteration(r, execution));
      }
    } catch (e_2_1) {
      e_2 = {error: e_2_1};
    } finally {
      try {
        if (_d && !_d.done && (_a = _b.return))
          _a.call(_b);
      } finally {
        if (e_2)
          throw e_2.error;
      }
    }
    r.pushes = [];
    r.nexts = [];
  }
  function reject(r) {
    if (r.state >= Rejected) {
      return;
    }
    if (r.state < Done) {
      finish(r);
    }
    r.state = Rejected;
  }
  function push(r, value) {
    swallow(value);
    if (r.pushes.length >= MAX_QUEUE_LENGTH) {
      throw new RepeaterOverflowError("No more than " + MAX_QUEUE_LENGTH + " pending calls to push are allowed on a single repeater.");
    } else if (r.state >= Stopped) {
      return Promise.resolve(void 0);
    }
    var valueP = r.pending === void 0 ? Promise.resolve(value) : r.pending.then(function() {
      return value;
    });
    valueP = valueP.catch(function(err) {
      if (r.state < Stopped) {
        r.err = err;
      }
      reject(r);
      return void 0;
    });
    var nextP;
    if (r.nexts.length) {
      var next_1 = r.nexts.shift();
      next_1.resolve(createIteration(r, valueP));
      if (r.nexts.length) {
        nextP = Promise.resolve(r.nexts[0].value);
      } else {
        nextP = new Promise(function(resolve) {
          return r.onnext = resolve;
        });
      }
    } else if (typeof r.buffer !== "undefined" && !r.buffer.full) {
      r.buffer.add(valueP);
      nextP = Promise.resolve(void 0);
    } else {
      nextP = new Promise(function(resolve) {
        return r.pushes.push({resolve, value: valueP});
      });
    }
    var floating = true;
    var next = {};
    var unhandled = nextP.catch(function(err) {
      if (floating) {
        throw err;
      }
      return void 0;
    });
    next.then = function(onfulfilled, onrejected) {
      floating = false;
      return Promise.prototype.then.call(nextP, onfulfilled, onrejected);
    };
    next.catch = function(onrejected) {
      floating = false;
      return Promise.prototype.catch.call(nextP, onrejected);
    };
    next.finally = nextP.finally.bind(nextP);
    r.pending = valueP.then(function() {
      return unhandled;
    }).catch(function(err) {
      r.err = err;
      reject(r);
    });
    return next;
  }
  function createStop(r) {
    var stop1 = stop.bind(null, r);
    var stopP = new Promise(function(resolve) {
      return r.onstop = resolve;
    });
    stop1.then = stopP.then.bind(stopP);
    stop1.catch = stopP.catch.bind(stopP);
    stop1.finally = stopP.finally.bind(stopP);
    return stop1;
  }
  function execute(r) {
    if (r.state >= Started) {
      return;
    }
    r.state = Started;
    var push1 = push.bind(null, r);
    var stop1 = createStop(r);
    r.execution = new Promise(function(resolve) {
      return resolve(r.executor(push1, stop1));
    });
    r.execution.catch(function() {
      return stop(r);
    });
  }
  var records = new WeakMap();
  var Repeater = function() {
    function Repeater2(executor, buffer) {
      records.set(this, {
        executor,
        buffer,
        err: void 0,
        state: Initial,
        pushes: [],
        nexts: [],
        pending: void 0,
        execution: void 0,
        onnext: NOOP,
        onstop: NOOP
      });
    }
    Repeater2.prototype.next = function(value) {
      swallow(value);
      var r = records.get(this);
      if (r === void 0) {
        throw new Error("WeakMap error");
      }
      if (r.nexts.length >= MAX_QUEUE_LENGTH) {
        throw new RepeaterOverflowError("No more than " + MAX_QUEUE_LENGTH + " pending calls to next are allowed on a single repeater.");
      }
      if (r.state <= Initial) {
        execute(r);
      }
      r.onnext(value);
      if (typeof r.buffer !== "undefined" && !r.buffer.empty) {
        var result = createIteration(r, r.buffer.remove());
        if (r.pushes.length) {
          var push_2 = r.pushes.shift();
          r.buffer.add(push_2.value);
          r.onnext = push_2.resolve;
        }
        return result;
      } else if (r.pushes.length) {
        var push_3 = r.pushes.shift();
        r.onnext = push_3.resolve;
        return createIteration(r, push_3.value);
      } else if (r.state >= Stopped) {
        finish(r);
        return createIteration(r, consumeExecution(r));
      }
      return new Promise(function(resolve) {
        return r.nexts.push({resolve, value});
      });
    };
    Repeater2.prototype.return = function(value) {
      swallow(value);
      var r = records.get(this);
      if (r === void 0) {
        throw new Error("WeakMap error");
      }
      finish(r);
      r.execution = Promise.resolve(r.execution).then(function() {
        return value;
      });
      return createIteration(r, consumeExecution(r));
    };
    Repeater2.prototype.throw = function(err) {
      var r = records.get(this);
      if (r === void 0) {
        throw new Error("WeakMap error");
      }
      if (r.state <= Initial || r.state >= Stopped || typeof r.buffer !== "undefined" && !r.buffer.empty) {
        finish(r);
        if (r.err == null) {
          r.err = err;
        }
        return createIteration(r, consumeExecution(r));
      }
      return this.next(Promise.reject(err));
    };
    Repeater2.prototype[Symbol.asyncIterator] = function() {
      return this;
    };
    Repeater2.race = race;
    Repeater2.merge = merge;
    Repeater2.zip = zip;
    Repeater2.latest = latest;
    return Repeater2;
  }();
  function getIterators(values, options) {
    var e_3, _a;
    var iters = [];
    var _loop_1 = function(value2) {
      if (value2 != null && typeof value2[Symbol.asyncIterator] === "function") {
        iters.push(value2[Symbol.asyncIterator]());
      } else if (value2 != null && typeof value2[Symbol.iterator] === "function") {
        iters.push(value2[Symbol.iterator]());
      } else {
        iters.push(function valueToAsyncIterator() {
          return __asyncGenerator(this, arguments, function valueToAsyncIterator_1() {
            return __generator(this, function(_a2) {
              switch (_a2.label) {
                case 0:
                  if (!options.yieldValues)
                    return [3, 3];
                  return [4, __await(value2)];
                case 1:
                  return [4, _a2.sent()];
                case 2:
                  _a2.sent();
                  _a2.label = 3;
                case 3:
                  if (!options.returnValues)
                    return [3, 5];
                  return [4, __await(value2)];
                case 4:
                  return [2, _a2.sent()];
                case 5:
                  return [2];
              }
            });
          });
        }());
      }
    };
    try {
      for (var values_1 = __values(values), values_1_1 = values_1.next(); !values_1_1.done; values_1_1 = values_1.next()) {
        var value = values_1_1.value;
        _loop_1(value);
      }
    } catch (e_3_1) {
      e_3 = {error: e_3_1};
    } finally {
      try {
        if (values_1_1 && !values_1_1.done && (_a = values_1.return))
          _a.call(values_1);
      } finally {
        if (e_3)
          throw e_3.error;
      }
    }
    return iters;
  }
  function race(contenders) {
    var _this = this;
    var iters = getIterators(contenders, {returnValues: true});
    return new Repeater(function(push2, stop2) {
      return __awaiter(_this, void 0, void 0, function() {
        var advance, stopped, finalIteration, iteration, i_1, _loop_2;
        return __generator(this, function(_a) {
          switch (_a.label) {
            case 0:
              if (!iters.length) {
                stop2();
                return [2];
              }
              stopped = false;
              stop2.then(function() {
                advance();
                stopped = true;
              });
              _a.label = 1;
            case 1:
              _a.trys.push([1, , 5, 7]);
              iteration = void 0;
              i_1 = 0;
              _loop_2 = function() {
                var j, iters_1, iters_1_1, iter;
                var e_4, _a2;
                return __generator(this, function(_b) {
                  switch (_b.label) {
                    case 0:
                      j = i_1;
                      try {
                        for (iters_1 = (e_4 = void 0, __values(iters)), iters_1_1 = iters_1.next(); !iters_1_1.done; iters_1_1 = iters_1.next()) {
                          iter = iters_1_1.value;
                          Promise.resolve(iter.next()).then(function(iteration2) {
                            if (iteration2.done) {
                              stop2();
                              if (finalIteration === void 0) {
                                finalIteration = iteration2;
                              }
                            } else if (i_1 === j) {
                              i_1++;
                              advance(iteration2);
                            }
                          }, function(err) {
                            return stop2(err);
                          });
                        }
                      } catch (e_4_1) {
                        e_4 = {error: e_4_1};
                      } finally {
                        try {
                          if (iters_1_1 && !iters_1_1.done && (_a2 = iters_1.return))
                            _a2.call(iters_1);
                        } finally {
                          if (e_4)
                            throw e_4.error;
                        }
                      }
                      return [4, new Promise(function(resolve) {
                        return advance = resolve;
                      })];
                    case 1:
                      iteration = _b.sent();
                      if (!(iteration !== void 0))
                        return [3, 3];
                      return [4, push2(iteration.value)];
                    case 2:
                      _b.sent();
                      _b.label = 3;
                    case 3:
                      return [2];
                  }
                });
              };
              _a.label = 2;
            case 2:
              if (!!stopped)
                return [3, 4];
              return [5, _loop_2()];
            case 3:
              _a.sent();
              return [3, 2];
            case 4:
              return [2, finalIteration && finalIteration.value];
            case 5:
              stop2();
              return [4, Promise.race(iters.map(function(iter) {
                return iter.return && iter.return();
              }))];
            case 6:
              _a.sent();
              return [7];
            case 7:
              return [2];
          }
        });
      });
    });
  }
  function merge(contenders) {
    var _this = this;
    var iters = getIterators(contenders, {yieldValues: true});
    return new Repeater(function(push2, stop2) {
      return __awaiter(_this, void 0, void 0, function() {
        var advances, stopped, finalIteration;
        var _this2 = this;
        return __generator(this, function(_a) {
          switch (_a.label) {
            case 0:
              if (!iters.length) {
                stop2();
                return [2];
              }
              advances = [];
              stopped = false;
              stop2.then(function() {
                var e_5, _a2;
                stopped = true;
                try {
                  for (var advances_1 = __values(advances), advances_1_1 = advances_1.next(); !advances_1_1.done; advances_1_1 = advances_1.next()) {
                    var advance = advances_1_1.value;
                    advance();
                  }
                } catch (e_5_1) {
                  e_5 = {error: e_5_1};
                } finally {
                  try {
                    if (advances_1_1 && !advances_1_1.done && (_a2 = advances_1.return))
                      _a2.call(advances_1);
                  } finally {
                    if (e_5)
                      throw e_5.error;
                  }
                }
              });
              _a.label = 1;
            case 1:
              _a.trys.push([1, , 3, 4]);
              return [4, Promise.all(iters.map(function(iter, i) {
                return __awaiter(_this2, void 0, void 0, function() {
                  var iteration, _a2;
                  return __generator(this, function(_b) {
                    switch (_b.label) {
                      case 0:
                        _b.trys.push([0, , 6, 9]);
                        _b.label = 1;
                      case 1:
                        if (!!stopped)
                          return [3, 5];
                        Promise.resolve(iter.next()).then(function(iteration2) {
                          return advances[i](iteration2);
                        }, function(err) {
                          return stop2(err);
                        });
                        return [4, new Promise(function(resolve) {
                          advances[i] = resolve;
                        })];
                      case 2:
                        iteration = _b.sent();
                        if (!(iteration !== void 0))
                          return [3, 4];
                        if (iteration.done) {
                          finalIteration = iteration;
                          return [2];
                        }
                        return [4, push2(iteration.value)];
                      case 3:
                        _b.sent();
                        _b.label = 4;
                      case 4:
                        return [3, 1];
                      case 5:
                        return [3, 9];
                      case 6:
                        _a2 = iter.return;
                        if (!_a2)
                          return [3, 8];
                        return [4, iter.return()];
                      case 7:
                        _a2 = _b.sent();
                        _b.label = 8;
                      case 8:
                        return [7];
                      case 9:
                        return [2];
                    }
                  });
                });
              }))];
            case 2:
              _a.sent();
              return [2, finalIteration && finalIteration.value];
            case 3:
              stop2();
              return [7];
            case 4:
              return [2];
          }
        });
      });
    });
  }
  function zip(contenders) {
    var _this = this;
    var iters = getIterators(contenders, {returnValues: true});
    return new Repeater(function(push2, stop2) {
      return __awaiter(_this, void 0, void 0, function() {
        var advance, stopped, iterations, values;
        return __generator(this, function(_a) {
          switch (_a.label) {
            case 0:
              if (!iters.length) {
                stop2();
                return [2, []];
              }
              stopped = false;
              stop2.then(function() {
                advance();
                stopped = true;
              });
              _a.label = 1;
            case 1:
              _a.trys.push([1, , 6, 8]);
              _a.label = 2;
            case 2:
              if (!!stopped)
                return [3, 5];
              Promise.all(iters.map(function(iter) {
                return iter.next();
              })).then(function(iterations2) {
                return advance(iterations2);
              }, function(err) {
                return stop2(err);
              });
              return [4, new Promise(function(resolve) {
                return advance = resolve;
              })];
            case 3:
              iterations = _a.sent();
              if (iterations === void 0) {
                return [2];
              }
              values = iterations.map(function(iteration) {
                return iteration.value;
              });
              if (iterations.some(function(iteration) {
                return iteration.done;
              })) {
                return [2, values];
              }
              return [4, push2(values)];
            case 4:
              _a.sent();
              return [3, 2];
            case 5:
              return [3, 8];
            case 6:
              stop2();
              return [4, Promise.all(iters.map(function(iter) {
                return iter.return && iter.return();
              }))];
            case 7:
              _a.sent();
              return [7];
            case 8:
              return [2];
          }
        });
      });
    });
  }
  function latest(contenders) {
    var _this = this;
    var iters = getIterators(contenders, {
      yieldValues: true,
      returnValues: true
    });
    return new Repeater(function(push2, stop2) {
      return __awaiter(_this, void 0, void 0, function() {
        var advance, advances, stopped, iterations_1, values_2;
        var _this2 = this;
        return __generator(this, function(_a) {
          switch (_a.label) {
            case 0:
              if (!iters.length) {
                stop2();
                return [2, []];
              }
              advances = [];
              stopped = false;
              stop2.then(function() {
                var e_6, _a2;
                advance();
                try {
                  for (var advances_2 = __values(advances), advances_2_1 = advances_2.next(); !advances_2_1.done; advances_2_1 = advances_2.next()) {
                    var advance1 = advances_2_1.value;
                    advance1();
                  }
                } catch (e_6_1) {
                  e_6 = {error: e_6_1};
                } finally {
                  try {
                    if (advances_2_1 && !advances_2_1.done && (_a2 = advances_2.return))
                      _a2.call(advances_2);
                  } finally {
                    if (e_6)
                      throw e_6.error;
                  }
                }
                stopped = true;
              });
              _a.label = 1;
            case 1:
              _a.trys.push([1, , 5, 7]);
              Promise.all(iters.map(function(iter) {
                return iter.next();
              })).then(function(iterations) {
                return advance(iterations);
              }, function(err) {
                return stop2(err);
              });
              return [4, new Promise(function(resolve) {
                return advance = resolve;
              })];
            case 2:
              iterations_1 = _a.sent();
              if (iterations_1 === void 0) {
                return [2];
              }
              values_2 = iterations_1.map(function(iteration) {
                return iteration.value;
              });
              if (iterations_1.every(function(iteration) {
                return iteration.done;
              })) {
                return [2, values_2];
              }
              return [4, push2(values_2.slice())];
            case 3:
              _a.sent();
              return [4, Promise.all(iters.map(function(iter, i) {
                return __awaiter(_this2, void 0, void 0, function() {
                  var iteration;
                  return __generator(this, function(_a2) {
                    switch (_a2.label) {
                      case 0:
                        if (iterations_1[i].done) {
                          return [2, iterations_1[i].value];
                        }
                        _a2.label = 1;
                      case 1:
                        if (!!stopped)
                          return [3, 4];
                        Promise.resolve(iter.next()).then(function(iteration2) {
                          return advances[i](iteration2);
                        }, function(err) {
                          return stop2(err);
                        });
                        return [4, new Promise(function(resolve) {
                          return advances[i] = resolve;
                        })];
                      case 2:
                        iteration = _a2.sent();
                        if (iteration === void 0) {
                          return [2, iterations_1[i].value];
                        } else if (iteration.done) {
                          return [2, iteration.value];
                        }
                        values_2[i] = iteration.value;
                        return [4, push2(values_2.slice())];
                      case 3:
                        _a2.sent();
                        return [3, 1];
                      case 4:
                        return [2];
                    }
                  });
                });
              }))];
            case 4:
              return [2, _a.sent()];
            case 5:
              stop2();
              return [4, Promise.all(iters.map(function(iter) {
                return iter.return && iter.return();
              }))];
            case 6:
              _a.sent();
              return [7];
            case 7:
              return [2];
          }
        });
      });
    });
  }

  // src/live_view.ts
  function mkHandle(url) {
    const webSocket = new WebSocket(url);
    const messages = new Repeater(async (push2, stop2) => {
      return new Promise((resolve) => {
        webSocket.addEventListener("close", (event) => resolve({type: "close", event}));
        webSocket.addEventListener("error", (event) => resolve({type: "error", event}));
        webSocket.addEventListener("message", (event) => {
          push2({type: "message", event});
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
  function parseWSMessage(wsMessage) {
    console.log("parsing");
    const parsed = JSON.parse(wsMessage.event.data);
    console.log(parsed);
    return parsed;
  }
  function applyPatch(currArray, patches) {
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
      throw new Error(`Assertion failed: ${currIndex} !== ${currArray.length}`);
    }
    return out;
  }
  async function attach(root, wsUrl) {
    const ws = mkHandle(wsUrl);
    let currClock = 0;
    let currArray;
    let cleanup = instrumentHsaction(root, (call) => {
      console.log("sending");
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
            if (msg[0] === "mount") {
              currArray = msg[1];
            } else {
              if (!currArray) {
                throw new Error("Tried to mount with no currArray.");
              }
              currArray = applyPatch(currArray, msg[1]);
            }
            const toMorph = `${currArray.join("")}`;
            console.log(root);
            console.log("morphing");
            console.log(toMorph);
            console.log(root.innerHTML);
            cleanup();
            cleanup = instrumentHsaction(root, (call) => {
              console.log("sending");
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

  // src/index.ts
  console.log("hello!!");
  console.log(attach(document.querySelector("#lvroot"), "ws://localhost:5000/liveview"));
})();
