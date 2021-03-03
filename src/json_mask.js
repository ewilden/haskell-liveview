"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.applyMask = void 0;
function applyMask(object, jsonMask) {
    if (!object) {
        return object;
    }
    if (!jsonMask) {
        return jsonMask;
    }
    if (!(typeof object === "object" || typeof object === "function")) {
        return object;
    }
    if (jsonMask === "deep") {
        return object;
    }
    const out = {};
    if (jsonMask === "shallow") {
        for (const key of Object.keys(object)) {
            let prop = object[key];
            if (typeof prop === "object" || typeof prop === "function") {
                prop = null;
            }
            out[key] = prop;
        }
        return out;
    }
    // typeof jsonMask === "object"
    for (const key of Object.keys(jsonMask)) {
        out[key] = applyMask(object[key], jsonMask[key]);
    }
    return out;
}
exports.applyMask = applyMask;
