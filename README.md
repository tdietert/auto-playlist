# auto-playlist
Uses the Spotify API to create a playlist of songs using preferences provided by user.

### Current State:
The app currently authenticates a client id and secret, and is able to make a request to the search endpoint of the spotify API. Ignore the Soundcloud code.

### To build:
1) Register a new application with Spotify at: https://developer.spotify.com/web-api/tutorial/ and follow the first bit of instructions to get a client-id and client-secret.

2) Make sure you have stack installed on your machine: https://docs.haskellstack.org/en/stable/README/

3) Consult src/Environment.hs to see how you should structure your top-level config file (I usually name it config.json).

#### To build & open app in browser:
```
$ make setup         -- run this the first time building the project (takes a long time)
-- | In separate terminals:
$ make backend       -- builds backend
$ auto-playlist-server config.json
-- | and
$ make frontend-open -- builds & opens app using google-chrome
```

#### Note:
Initially I needed to clone `reflex` and `reflex-dom` packages, because I needed fine grained access to the `XhrRequest` datatype so that I could set `xhrWithCredential` to `True` because the client used to test the server was being served statically. This has since changed, and the frontend is served by the same server that the frontend code sends (REST) API requests to, so CORS is _probably_ not needed, and I can move back to `reflex` and `reflex-dom` versions that aren't locally cloned... TODO.


