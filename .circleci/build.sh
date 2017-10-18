#!/usr/bin/env bash
# vim: sw=2 et

set -euo pipefail

echo 'BUILD_SPHINX_HTML = NO' > mk/validate.mk
echo 'BUILD_SPHINX_PDF = NO' >> mk/validate.mk

export THREADS=8
export SKIP_PERF_TESTS=YES
export VERBOSE=2

if [[ -n ${TARGET:-} ]]; then
  if [[ $(uname) != Linux ]]; then
    fail "uname=$(uname) not supported for cross-compilation"
  fi
  if [[ $target != FreeBSD ]]; then
    fail "TARGET=$TARGET not supported"
  fi
  echo 'HADDOCK_DOCS = NO' >> mk/validate.mk
  echo 'WERROR=' >> mk/validate.mk
  export PATH=/opt/ghc/bin:$PATH
  ./boot
  config_args=--target=x86_64-unknown-freebsd10 ./validate --quiet --build-only
else
  ./validate --fast --quiet
fi
