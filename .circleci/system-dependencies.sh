#!/usr/bin/env bash
# vim: sw=2 et
set -euo pipefail

target=${TARGET:-$(uname)}
case "$target" in
  Linux)
    apt-get update -qq
    apt-get install -qy git openssh-client make automake autoconf gcc perl python3 texinfo
    ;;
  FreeBSD)
    # yes, it's cross-compiling
    add-apt-repository ppa:hvr/ghc
    apt-get update -qq
    apt-get install -qy ghc-8.0.2 alex happy ncurses-dev
    ;;
  Darwin)
    brew install ghc
    ;;
esac

