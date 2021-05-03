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