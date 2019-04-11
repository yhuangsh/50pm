#!/bin/sh

if [ -z $1 ] 
then
  echo "missing release tag name"
  exit
else
  docker pull yhuangsh/50pm:latest
  docker tag yhuangsh/50pm:latest yhuangsh/50pm:$1
  docker push yhuangsh/50pm:$1
  kubectl set image deploy/fiftypm fiftypm=yhuangsh/50pm:$1 --record
fi
