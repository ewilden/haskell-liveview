"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.attach = void 0;
const hsaction_1 = require("./hsaction");
class WebSocketApp {
}
async function attach(root, wsUrl) {
    hsaction_1.instrumentHsaction(root, console.log);
}
exports.attach = attach;
