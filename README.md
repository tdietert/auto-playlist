# auto-playlist
Uses the Spotify API to create a playlist of songs using preferences provided by user.

### Current State:
The app currently authenticates a client id and secret, and is able to make a request to the search endpoint of the spotify API. Ignore the Soundcloud code.

### To build:
1) Register a new application with Spotify at: https://developer.spotify.com/web-api/tutorial/
   and follow the first bit of instructions to get a client-id and client-secret.

2) Make sure you have stack installed on your machine: https://docs.haskellstack.org/en/stable/README/

```
$ stack setup 
$ stack install -j 4
$ auto-playlist-exe <client-id> <client-secret> <searchQuery> <searchType> 
```

where type is one of "artist", "track", "album", or "playlist"


