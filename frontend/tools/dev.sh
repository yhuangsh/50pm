#!/bin/sh
docker run \
    --rm \
    --net dev-net \
    --name 50pm-dev \
    --ip 172.28.0.30 \
    --hostname 50pm \
    -v `pwd`:/react \
    -p 8080:3000 \
    -it \
    node8-npm6.9.0:latest 
