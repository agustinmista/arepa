#!/bin/bash

HERE=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)

IMAGE_NAME=arepa
IMAGE_VER=0.1

docker build \
  -f $HERE/Dockerfile \
  --build-arg UID=$(id -u) \
  --build-arg GID=$(id -g) \
  -t $IMAGE_NAME:$IMAGE_VER \
  $HERE/..