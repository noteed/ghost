#! /usr/bin/env bash

STACK_IMAGE=${1:-7.8.4}

docker run \
  -v `pwd`:/home/gusdev/ghost \
  images.reesd.com/reesd/stack:$STACK_IMAGE \
  cabal install ghost/ghost.cabal
