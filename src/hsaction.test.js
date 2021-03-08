"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const hsaction_1 = require("./hsaction");
function tick(ms = 1) {
    return new Promise(ok => setTimeout(ok, ms));
}
test('instrumentHsaction', async () => {
    const DEBOUNCE_DURATION = 50;
    document.body.innerHTML =
        `<div id="root">
    <button id="basic_click"
            hsaction="click:basic_click"
            hsvalue-foo="bar">
    </button>
    <ul id="list1">
      <button id="debounce_click" 
              hsaction="click:debounce_click"
              hsdebounce="${DEBOUNCE_DURATION}">
      </button>
      <button id="debounce_blur_focus" 
              hsaction="focus:debounce_blur_focus;keyup:other_event" 
              hsdebounce="blur">
      </button>
    </ul>
  </div>`;
    const events = [];
    const cleanup = hsaction_1.instrumentHsaction(document.body, e => {
        events.push([e, Date.now()]);
    });
    expect(events.length).toBe(0);
    document.getElementById('basic_click').click();
    const basicClickTime = Date.now();
    expect(events.length).toBe(1);
    expect(events[0][0]).toEqual({ action: 'basic_click', payload: { foo: 'bar' } });
    expect(events[0][1] - basicClickTime).toBeLessThan(16);
    document.getElementById('debounce_click').click();
    document.getElementById('debounce_click').click();
    document.getElementById('debounce_click').click();
    document.getElementById('debounce_click').click();
    const debounceClickTime = Date.now();
    expect(events.length).toBe(1);
    await tick();
    expect(events.length).toBe(1);
    await tick(DEBOUNCE_DURATION + 50);
    expect(events.length).toBe(2);
    expect(events[1][0]).toEqual({ action: 'debounce_click', payload: {} });
    expect(events[1][1] - (debounceClickTime + DEBOUNCE_DURATION)).toBeLessThan(16);
    document.getElementById('debounce_blur_focus').focus();
    document.getElementById('debounce_blur_focus').focus();
    document.getElementById('debounce_blur_focus').focus();
    document.getElementById('debounce_blur_focus').focus();
    document.getElementById('debounce_blur_focus').focus();
    expect(events.length).toBe(2);
    await tick(50);
    expect(events.length).toBe(2);
    document.getElementById('debounce_blur_focus').blur();
    const debounceBlurTime = Date.now();
    await tick(50);
    expect(events.length).toBe(3);
    expect(events[2][0]).toEqual({ action: 'debounce_blur_focus', payload: {} });
    expect(events[2][1] - debounceBlurTime).toBeLessThan(16);
    const eventsLengthBeforeCleanup = events.length;
    cleanup();
    document.getElementById('basic_click').click();
    document.getElementById('debounce_click').click();
    await tick(DEBOUNCE_DURATION + 50);
    expect(events.length).toBe(eventsLengthBeforeCleanup);
});
