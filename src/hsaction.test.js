"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const hsaction_1 = require("./hsaction");
function tick(ms = 1) {
    return new Promise(ok => setTimeout(ok, ms));
}
test('is a function', async () => {
    const DEBOUNCE_DURATION = 10;
    document.body.innerHTML =
        `<div id="root">
    <ul id="list1">
      <li id="clicktarget" hsaction="click:hello_click">Hello</li>
      <li hsaction="focus:test_focus;blur:hello_blur" hsvalue-foo="foo">Test</li>
    </ul>
  </div>`;
    const events = [];
    expect(Array.from(document.body.querySelectorAll('[hsaction]')).length).toBe(2);
    const cleanup = hsaction_1.instrumentHsaction(document.body, e => {
        console.log('consumer');
        events.push(e);
    });
    expect(typeof cleanup).toBe('function');
    // document.getElementById('clicktarget')!.addEventListener('click', (e) => {events.push({action: 'foo', payload: {foo: 'foo'}});});
    // expect(document.getElementById('clicktarget')!.onclick).toBeTruthy();
    document.getElementById('clicktarget').click();
    await tick(DEBOUNCE_DURATION);
    expect(events.length).toBe(1);
});
