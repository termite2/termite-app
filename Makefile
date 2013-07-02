ifeq (${COMMON},)
$(warning "Must set the common directory. Source the develop script at the root of the repository or set the COMMON environment variable")
ROOT=../../
COMMON=${ROOT}/apps/common
endif

include ${COMMON}/Makefile.inc

GHCINC= ${BDD_PATH} ${ABSTRACT2_PATH} ${TSL2_PATH} ${TSL2_PATH}/frontend ${TSL2_PATH}/solver ${TSL2_PATH}/internal ${DEBUGGER2_PATH} ${TSL2_PATH}/abstract ${BDD_PATH} ${UTIL_PATH} ${GRAPHDRAW_PATH} ${ABSTRACT_PATH} ${TSL_PATH} ${CUDD_HASKELL_PATH} ${HAST_PATH}
LIBS=${CUDDLIBS} ${CUDDHLIB} 
LIBPATHS= ${CUDDLIBPATHS} \
	 ${CUDD_HASKELL_PATH}
TARGET=tsl2.hs
CLIBS=${LIBS} stdc++
GHC_FLAGS+=-o $(ROOT)/bin/tsl2 -Wall -fno-warn-incomplete-patterns -fno-warn-missing-signatures  #-fforce-recomp # -prof -auto-all -rtsopts # -fforce-recomp 

CABAL_PACKAGES= 

EXTRA_LIB_DIRS=

cabal: 
	cabal-dev install $(CABAL_PACKAGES) $(EXTRA_LIB_DIRS)
	cp cabal-dev/bin/tsl2 $(ROOT)/bin/tsl2

default:
	ghc --make -c -fcontext-stack=64 -O2 ${GHC_FLAGS} ${GHCINC:%=-i%} ${TARGET} ${LIBPATHS:%=-L%} ${LIBS:%=-l%}
	ghc --make -fcontext-stack=64    -O2 ${GHC_FLAGS} ${GHCINC:%=-i%} ${TARGET} ${LIBPATHS:%=-L%} ${LIBS:%=-l%} ${CLIBS:%=-optl-l%} 

prof:
	ghc --make -c -fcontext-stack=64 -O2 ${GHC_FLAGS} ${GHCINC:%=-i%} ${TARGET} ${LIBPATHS:%=-L%} ${LIBS:%=-l%}
	ghc --make -c -fcontext-stack=64 -osuf oprof -prof -auto-all -rtsopts -O2 ${GHC_FLAGS} ${GHCINC:%=-i%} ${TARGET} ${LIBPATHS:%=-L%} ${LIBS:%=-l%}
	ghc --make -fcontext-stack=64 -osuf oprof -prof -auto-all -rtsopts -O2 ${GHC_FLAGS} ${GHCINC:%=-i%} ${TARGET} ${LIBPATHS:%=-L%} ${LIBS:%=-l%} ${CLIBS:%=-optl-l%} 
