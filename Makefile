
.PHONY: all backend frontend frontend-open setup

all: backend frontend

backend: 
	stack install -j4

frontend:
	cd frontend && stack build -j4 
	mkdir -p public/appjs
	cd frontend && cp -R `stack path --dist-dir`/build/auto-playlist-app/auto-playlist-app.jsexe/* ../public/appjs

frontend-open: frontend
	cd ./frontend && google-chrome http://localhost:3000/

setup: backend-setup frontend-setup

backend-setup:
	stack setup
	stack build -j4

frontend-setup:
	cd frontend 
	stack install gtk2hs-buildtools 
	stack build -j4 
