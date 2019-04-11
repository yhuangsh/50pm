#!/bin/sh

# On Travis, must run this script after dockerize & deploy.staging
if [ ${TRAVIS_TAG:0:1} == 'v' ] 
then
  docker tag yhuangsh/50pm:latest yhuangsh/50pm:$TRAVIS_TAG
  docker push yhuangsh/50pm:$TRAVIS_TAG
  ./kubectl set image deploy/fiftypm fiftypm=yhuangsh/50pm:$TRAVIS_TAG --record
else
  echo "This is not a release build, skipping production deployment"
fi
