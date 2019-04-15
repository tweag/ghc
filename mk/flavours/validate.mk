SRC_HC_OPTS        = -O0 -H64m
SRC_HC_OPTS_STAGE1 = -fllvm-fill-undef-with-garbage   # See #11487
GhcStage1HcOpts    = -O2 -DDEBUG
GhcStage2HcOpts    = -O -dcore-lint -dno-debug-output
GhcLibHcOpts       = -O -dcore-lint -dno-debug-output
BUILD_PROF_LIBS    = NO
SplitSections      = NO
HADDOCK_DOCS       = NO
BUILD_SPHINX_HTML  = NO
BUILD_SPHINX_PDF   = NO

ifeq "$(ValidateHpc)" "YES"
GhcStage2HcOpts   += -fhpc -hpcdir $(TOP)/testsuite/hpc_output/
endif

ifeq "$(ValidateSpeed)" "SLOW"
GhcStage2HcOpts   += -DDEBUG
endif

ifeq "$(ValidateSpeed)" "SLOW"
BUILD_PROF_LIBS    = YES
endif

ifneq "$(ValidateSpeed)" "FAST"
BUILD_EXTRA_PKGS   = YES
endif

WERROR             = -Werror

# DO NOT EDIT THIS FILE! Instead, create a file mk/validate.mk, whose settings
# will override these. See also mk/custom-settings.mk.
#
#
# (Note: these comments are at the end of this file, to make it easier to diff
# this file with other build flavours.)
#
#
# Note [validate build settings]
#
# Using GhcStage2HcOpts=-O (rather than -O0) here bringes my validate down from
# 22mins to 16 mins. Compiling stage2 takes longer, but we gain a faster
# haddock, faster running of the tests, and faster building of the utils to be
# installed
#
# dblatex with miktex under msys/mingw can't build the PS and PDF docs,
# and just building the HTML docs is sufficient to check that the
# markup is correct, so we turn off PS and PDF doc building when
# validating.
#
# We set BUILD_EXTRA_PKGS=YES to build the "extra" packages (see ./packages),
# so that we can test them.
