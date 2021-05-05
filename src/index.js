import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

const websocket = new WebSocket('ws://localhost:8000/weechat');
websocket.binaryType = 'arraybuffer';

websocket.onopen = () => {
  websocket.send('init password=test\n');
  websocket.send('info version\n');
}

websocket.addEventListener("message", event => {
  // Send message through the port as Int array, as Elm doesn't support ByteArrays through ports.
  const bytes = new Uint8Array(event.data);
  app.ports.weechatReceive.send(Array.from(bytes));
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
