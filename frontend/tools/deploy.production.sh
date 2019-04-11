#!/bin/sh

# On Travis, must run this script after dockerize & deploy.staging
if [ -n $TRAVIS_TAG ] 
then
  docker tag yhuangsh/50pm:latest yhuangsh/50pm:$TRAVIS_TAG
  docker push yhuangsh/50pm:$TRAVIS_TAG
  ./kubectl --kubeconfig=./kc set image deploy/fiftypm fiftypm=yhuangsh/50pm:$TRAVIS_TAG --record
else
  echo "This is not a release build, skipping production deployment"
fi
