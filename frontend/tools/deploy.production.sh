#!/bin/sh

# On Travis, must run this script after dockerize & deploy.staging

if [ -z $TRAVIS_TAG ] 
then
  echo "This is not a release build, skipping production deployment"
else
  docker tag yhuangsh/50pm:latest yhuangsh/50pm:$TRAVIS_TAG
  docker push yhuangsh/50pm:$TRAVIS_TAG
  ./kubectl --kubeconfig=./kc set image deploy/fiftypm fiftypm=yhuangsh/50pm:$TRAVIS_TAG --record
fi
