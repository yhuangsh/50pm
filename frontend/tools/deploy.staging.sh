#!/bin/sh

# deploy-staging is always run
./kubectl --kubeconfig=./kubectl delete svc/fiftypm-staging
./kubectl --kubeconfig=./kubectl delete deploy/fiftypm-staging
./kubectl --kubeconfig=./kubectl create -f frontend/tools/deploy.staging.yml