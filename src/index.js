import './reset.css';
import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root')
});

// Send to sessionStorage.
app.ports.storeSession.subscribe(data => {
  console.log('Storing session data:', data);
  sessionStorage.setItem('session', data);
});

// Load from sesionStorage.
app.ports.loadSession.subscribe(() => {
  console.log('Loading session data:', sessionStorage.getItem('session'));
  app.ports.receiveSession.send(sessionStorage.getItem('session'));
});

// Connect to a websocket by sending socket address to a port.
app.ports.connectWebSocket.subscribe(address => {
  const websocket = new WebSocket(address);
  websocket.binaryType = 'arraybuffer';

  // Receive messages from Weechat to Elm.
  websocket.addEventListener("message", event => {
    // Send message through the port as Int array, as Elm doesn't support ByteArrays through ports.
    const bytes = new Uint8Array(event.data);
    app.ports.weechatReceive.send(Array.from(bytes));
  });

  // Send messages from Elm to Weechat.
  app.ports.weechatSend.subscribe(message => {
    websocket.send(message);
  });

  // Receive websocket state to Elm.
  websocket.onopen = () => app.ports.socketStatus.send(true);
  websocket.onerror = () => app.ports.socketStatus.send(false);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
