#!/bin/sh
docker run \
    --rm \
    --net dev-net \
    --name fiftypm_api-staging-0 \
    --ip 172.28.0.12 \
    --hostname fiftypm_api-staging-0.fiftypm_api-staging.default.svc.cluster.local \
    --add-host fiftypm_api-staging-0.fiftypm_api-staging.default.svc.cluster.local:172.28.0.12 \
    --add-host fiftypm_api-staging-1.fiftypm_api-staging.default.svc.cluster.local:172.28.0.13 \
    --add-host fiftypm_api-staging-2.fiftypm_api-staging.default.svc.cluster.local:172.28.0.14 \
    -v `pwd`:/project \
    -p 8000:8000 \
    -it \
    yhuangsh/dev-alpine-erlang:latest