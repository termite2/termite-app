ifeq (${COMMON},)
$(warning "Must set the common directory. Source the develop script at the root of the repository or set the COMMON environment variable")
ROOT=../../
COMMON=${ROOT}/apps/common
endif

include ${COMMON}/Makefile.inc

GHCINC= ${BDD_PATH} ${ABSTRACT2_PATH} ${TSL2_PATH} ${TSL2_PATH}/frontend ${TSL2_PATH}/solver ${TSL2_PATH}/internal ${DEBUGGER2_PATH} ${TSL2_PATH}/abstract ${BDD_PATH} ${UTIL_PATH} ${GRAPHDRAW_PATH} ${ABSTRACT_PATH} ${TSL_PATH} ${CUDD_HASKELL_PATH}
LIBS=${CUDDLIBS} ${CUDDHLIB} 
LIBPATHS= ${CUDDLIBPATHS} \
	 ${CUDD_HASKELL_PATH}
TARGET=tsl2.hs
CLIBS=${LIBS} stdc++
GHC_FLAGS+=-o $(ROOT)/bin/tsl2 # -prof -auto-all -rtsopts # -fforce-recomp 

CABAL_PACKAGES= 

EXTRA_LIB_DIRS=

cabal: 
	cabal-dev install $(CABAL_PACKAGES) $(EXTRA_LIB_DIRS)
	cp cabal-dev/bin/tsl2 $(ROOT)/bin/tsl2

prof:
	ghc --make -c -fcontext-stack=64 -O2 ${GHC_FLAGS} ${GHCINC:%=-i%} ${TARGET} ${LIBPATHS:%=-L%} ${LIBS:%=-l%}
	ghc --make -fcontext-stack=64 -osuf oprof -prof -auto-all -rtsopts -O2 ${GHC_FLAGS} ${GHCINC:%=-i%} ${TARGET} ${LIBPATHS:%=-L%} ${LIBS:%=-l%} -no-link
	ghc --make -fcontext-stack=64 -osuf oprof -prof -auto-all -rtsopts -O2 ${GHC_FLAGS} ${GHCINC:%=-i%} ${TARGET} ${LIBPATHS:%=-L%} ${LIBS:%=-l%} ${CLIBS:%=-optl-l%} 


#-rtsopts=all -debug
#GHC_FLAGS+=-o $(ROOT)/bin/termite -prof -auto-all -caf-all #-fforce-recomp 


#CFLAGS=-g
#TARGET=termite
#
#WHERE=../../3rd_party/lib/cudd-2.4.2
#INCLUDE=-I$(WHERE)/include
#FFI=../../lib/CUDD_haskell
#DEBUGLIB=../../lib/debug
#ABSTRACTLIB=../../lib/abstract
#SYNTHLIB=../../lib/synthesis
#FSM_LIB=../../lib/FSM_draw
#TSL_COMPILER=../../lib/tsl
#
#GHCINC=$(FFI):$(TSL_COMPILER):$(DEBUGLIB):$(ABSTRACTLIB):$(SYNTHLIB):$(FSM_LIB)
#
#LIBS=-L$(WHERE)/dddmp -L$(WHERE)/cudd -L$(WHERE)/mtr -L$(WHERE)/st -L$(WHERE)/util -L$(WHERE)/epd -L$(FFI)
#
#LIBNAMES=-ldddmp -lcudd -lmtr -lst -lutil -lepd -lcuddwrap
#
#CLIBNAMES=-optl-lcudd -optl-ldddmp -optl-lmtr -optl-lst -optl-lutil -optl-lepd
#
##make -C $FFI
#
#
#all: $(TARGET)
#
#debug: 
#	ghci $(TARGET).hs $(INCLUDE) $(WHERE)/dddmp/*.o \
#    		  $(WHERE)/cudd/*.o \
#		  $(WHERE)/mtr/*.o \
#		  $(WHERE)/st/*.o \
#		  $(WHERE)/util/safe_mem.o \
#		  $(WHERE)/util/cpu_time.o \
#		  $(WHERE)/util/datalimit.o \
#		  $(WHERE)/epd/*.o \
#		  $(FFI)/*.o \
#		  -i../../lib/tsl/:../../lib/CUDD_haskell:../../lib/abstract\
#		  -fbyte-code \
#		  -fbreak-on-error
#
#.PHONY: $(TARGET)
#$(TARGET):
#	ghc --make -debug -ferror-spans -O2 -i$(GHCINC) $(TARGET).hs $(INCLUDE) $(LIBS) $(LIBNAMES) $(CLIBNAMES)
#
#clean:
#	rm -f *.o  *.hi $(TARGET)
