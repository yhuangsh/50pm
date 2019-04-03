#!/bin/sh

# run from frontend/
cp config/nginx.default.conf 50pm/build
cp tools/Dockerfile.nginx-static 50pm/build
docker build -t yhuangsh/50pm -f 50pm/build/Dockerfile.nginx-static 50pm/build
docker login $DOCKER_IO_CREDENTIAL
docker push yhuangsh/50pm 