type JsonMask = "deep" | "shallow" | Record<string, object> | null | undefined;

export function applyMask(object: any, jsonMask: JsonMask) {
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
  const out: any = {};
  if (jsonMask === "shallow") {
    for (const key of Object.keys(object as object)) {
      let prop: any = (object as any)[key];
      if (typeof prop === "object" || typeof prop === "function") {
        prop = null;
      }
      out[key] = prop;
    }
    return out;
  }
  // typeof jsonMask === "object"
  for (const key of Object.keys(jsonMask)) {
    out[key] = applyMask(object[key], jsonMask[key] as JsonMask);
  }
  return out;
}