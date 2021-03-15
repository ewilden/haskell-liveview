import * as liveView from './live_view';
import {Monad} from 'hkts/src';

console.log('hello!!');
console.log(liveView.attach(document.querySelector('#lvroot')!, "ws://localhost:5000/liveview"));