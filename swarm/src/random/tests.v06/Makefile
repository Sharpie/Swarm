.SUFFIXES: .m
.m.o:
	$(CC) -c $(CFLAGS) $<

CC=gcc
INCDIRS=-I.. -I../../../include
LIBDIRS=-L../../../lib
CFLAGS=-g -W -Wno-import $(INCDIRS)


RANDOMLIB=../../../lib/librandom.a

PROGRAMS=checkPMMLCG1 checkLCG1 chiSquare coinFlip printBits printPairs

all: $(PROGRAMS)
clean:
	rm *.o $(PROGRAMS)

checkPMMLCG1: checkPMMLCG1.o $(RANDOMLIB)
	$(CC) $(CFLAGS) $(LIBDIRS) -o checkPMMLCG1 checkPMMLCG1.o $(RANDOMLIB) -lm -lobjc
checkLCG1: checkLCG1.o $(RANDOMLIB)
	$(CC) $(CFLAGS) $(LIBDIRS) -o checkLCG1 checkLCG1.o $(RANDOMLIB) -lm -lobjc
chiSquare: chiSquare.o $(RANDOMLIB) genSelect.o
	$(CC) $(CFLAGS) $(LIBDIRS) -o chiSquare chiSquare.o genSelect.o $(RANDOMLIB) -lm -lobjc
coinFlip: coinFlip.o $(RANDOMLIB) genSelect.o
	$(CC) $(CFLAGS) $(LIBDIRS) -o coinFlip coinFlip.o genSelect.o $(RANDOMLIB) -lm -lobjc
printBits: printBits.o $(RANDOMLIB) genSelect.o
	$(CC) $(CFLAGS) $(LIBDIRS) -o printBits printBits.o genSelect.o $(RANDOMLIB) -lm -lobjc
printPairs: printPairs.o $(RANDOMLIB) genSelect.o
	$(CC) $(CFLAGS) $(LIBDIRS) -o printPairs printPairs.o genSelect.o $(RANDOMLIB) -lm -lobjc

checkPMMLCG1.o: checkPMMLCG1.m
checkLCG1.o: checkLCG1.m
chiSquare.o: chiSquare.m
coinFlip.o: coinFlip.m
printBits.o: printBits.m
genSelect.o: genSelect.m
printPairs.o: printPairs.m
