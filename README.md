# mopo

This is an IRC client using Weechat Relay server. Written in Elm.

### Development
```
elm make
elm-app start
```

### Setting up Weechat for testing

Install Weechat. For example on MacOS:
```
brew install weechat
```

Start weechat
```
weechat
```

Set up testing relay server.
```
/set relay.network.password test
/relay add weechat 8000
```