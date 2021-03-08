import {CallConsumer, CleanupCallback, instrumentHsaction} from './hsaction';
import morphdom from 'morphdom';

class WebSocketApp {

}

export async function attach(root: Element, wsUrl: string): Promise<void> {
  instrumentHsaction(root, console.log);
}