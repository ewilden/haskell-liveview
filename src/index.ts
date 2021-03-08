import * as liveView from './live_view';
import {Monad} from 'hkts/src';

console.log(liveView.attach(document.body, "ws://localhost:5000"));