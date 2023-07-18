#!/bin/bash

IMAGE_NAME=arepa
IMAGE_VER=0.1

docker run \
  --rm \
  -it \
  --volume $PWD:/workspace \
  $IMAGE_NAME:$IMAGE_VER -- $@