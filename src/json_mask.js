"use strict";
exports.__esModule = true;
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
    var out = {};
    if (jsonMask === "shallow") {
        for (var _i = 0, _a = Object.keys(object); _i < _a.length; _i++) {
            var key = _a[_i];
            var prop = object[key];
            if (typeof prop === "object" || typeof prop === "function") {
                prop = null;
            }
            out[key] = prop;
        }
        return out;
    }
    // typeof jsonMask === "object"
    for (var _b = 0, _c = Object.keys(jsonMask); _b < _c.length; _b++) {
        var key = _c[_b];
        out[key] = applyMask(object[key], jsonMask[key]);
    }
    return out;
}
exports.applyMask = applyMask;
