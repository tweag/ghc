#!/usr/bin/env bash
# vim: sw=2 et

set -euo pipefail

fail() {
  echo "ERROR: $*" >&2
  exit 1
}

echo 'BUILD_SPHINX_HTML = NO' > mk/validate.mk
echo 'BUILD_SPHINX_PDF = NO' >> mk/validate.mk

export THREADS=8
export SKIP_PERF_TESTS=YES
export VERBOSE=2

case "$(uname)" in
  Linux)
    if [[ -n ${TARGET:-} ]]; then
      if [[ $TARGET = FreeBSD ]]; then
        # cross-compiling to FreeBSD
        echo 'HADDOCK_DOCS = NO' >> mk/validate.mk
        echo 'WERROR=' >> mk/validate.mk
        export PATH=/opt/ghc/bin:$PATH
        ./boot
        config_args=--target=x86_64-unknown-freebsd10 ./validate --quiet --build-only
      else
        fail "TARGET=$target not supported"
      fi
    else
      ./validate --fast --quiet
    fi
    ;;
  Darwin)
    if [[ -n ${TARGET:-} ]]; then
      fail "uname=$(uname) not supported for cross-compilation"
    fi
    ./validate --fast --quiet
    ;;
  *)
    fail "uname=$(uname) not supported"
esac
