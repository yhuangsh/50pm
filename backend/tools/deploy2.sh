#!/bin/sh

#!/bin/sh
docker run \
    --rm \
    --net dev-net \
    --name fiftypm_api-staging-2 \
    --ip 172.28.0.14 \
    --hostname fiftypm_api-staging-2.fiftypm_api-staging.default.svc.cluster.local \
    --add-host fiftypm_api-staging-0.fiftypm_api-staging.default.svc.cluster.local:172.29.0.2 \
    --add-host fiftypm_api-staging-1.fiftypm_api-staging.default.svc.cluster.local:172.29.0.3 \
    --add-host fiftypm_api-staging-2.fiftypm_api-staging.default.svc.cluster.local:172.29.0.4 \
    -p 8002:8000 \
    -it yhuangsh/fiftypm_api:latest \
    /deploy/fiftypm_api-staging/bin/fiftypm_api-staging console