#!/bin/sh
docker run \
    --rm \
    --net dev-net \
    --name fiftypm_api-stg-0 \
    --ip 172.28.0.12 \
    --hostname fiftypm_api-stg-0.fiftypm_api-stg.default.svc.cluster.local \
    --add-host fiftypm_api-stg-0.fiftypm_api-stg.default.svc.cluster.local:172.28.0.12 \
    --add-host fiftypm_api-stg-1.fiftypm_api-stg.default.svc.cluster.local:172.28.0.13 \
    --add-host fiftypm_api-stg-2.fiftypm_api-stg.default.svc.cluster.local:172.28.0.14 \
    -v `pwd`:/project \
    -p 8000:80 \
    -it \
    yhuangsh/fiftypm_api_dev:latest