#!/bin/sh

# deploy-staging is always run
./kubectl --kubeconfig=./kc delete svc/fiftypm-staging
./kubectl --kubeconfig=./kc delete deploy/fiftypm-staging
./kubectl --kubeconfig=./kc create -f tools/deploy.staging.yml