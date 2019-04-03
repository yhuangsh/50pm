#!/bin/sh

# run from frontend/
cp config/nginx.default.conf 50pm/build
docker build -t yhuangsh/50pm -f tools/Dockerfile.nginx-static 50pm/build
docker push yhuangsh/50pm