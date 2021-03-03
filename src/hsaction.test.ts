import {ActionCall, instrumentHsaction} from './hsaction';

function tick(ms: number = 1): Promise<void> {
  return new Promise(ok => setTimeout(ok, ms));
}

test('is a function', async () => {
  const DEBOUNCE_DURATION = 10;
  document.body.innerHTML = 
  `<div id="root">
    <ul id="list1">
      <li id="clicktarget" hsaction="click:hello_click" hsdebounce="${DEBOUNCE_DURATION}">Hello</li>
      <li hsaction="focus:test_focus;blur:hello_blur" hsvalue-foo="foo">Test</li>
    </ul>
  </div>`
  const events: ActionCall[] = [];
  expect(Array.from(document.body.querySelectorAll('[hsaction]')).length).toBe(2);
  const cleanup = instrumentHsaction(document.body, e => {
    // console.log('consumer');
    events.push(e);
  });
  expect(typeof cleanup).toBe('function');
  // document.getElementById('clicktarget')!.addEventListener('click', (e) => {events.push({action: 'foo', payload: {foo: 'foo'}});});
  // expect(document.getElementById('clicktarget')!.onclick).toBeTruthy();
  document.getElementById('clicktarget')!.click();
  await tick(DEBOUNCE_DURATION + 50);
  expect(events.length).toBe(1);
});