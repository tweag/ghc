#!/usr/bin/env bash
# vim: sw=2 et

set -euo pipefail

fail() {
  echo "ERROR: $*" >&2
  exit 1
}

echo 'BUILD_SPHINX_HTML = NO' > mk/validate.mk
echo 'BUILD_SPHINX_PDF = NO' >> mk/validate.mk

cat > mk/build.mk <<EOF
V=1
HADDOCK_DOCS=YES
LATEX_DOCS=YES
HSCOLOUR_SRCS=YES
BUILD_DOCBOOK_HTML=YES
BeConservative=YES
EOF


export THREADS=8
export SKIP_PERF_TESTS=YES
export VERBOSE=2

case "$(uname)" in
  Linux)
    if [[ -n ${TARGET:-} ]]; then
      if [[ $TARGET = FreeBSD ]]; then
        # cross-compiling to FreeBSD
        echo 'HADDOCK_DOCS = NO' >> mk/build.mk
        echo 'WERROR=' >> mk/build.mk
        export PATH=/opt/ghc/bin:$PATH
        export config_args=
        ./boot
        ./configure --target=x86_64-unknown-freebsd10
        make -j8
        make fasttest
        make binary-dist
      else
        fail "TARGET=$target not supported"
      fi
    else
      ./boot
      ./configure
      make -j8
      make fasttest
      make binary-dist
    fi
    ;;
  Darwin)
    if [[ -n ${TARGET:-} ]]; then
      fail "uname=$(uname) not supported for cross-compilation"
    fi
    ./boot
    ./configure
    make -j8
    make fasttest
    make binary-dist
    ;;
  *)
    fail "uname=$(uname) not supported"
esac
