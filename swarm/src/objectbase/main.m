// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "SwarmObject.h"
#import "Probe.h"
#import "ProbeMap.h"
#import "ProbeMaker.h"
#import <collections.h>

@interface TestObject : SwarmObject {
  int firstInt;
  long long longLong;
  char firstChar;
  char secondChar;
  int secondInt;
}
@end

@implementation TestObject
@end

int
main() {
  TestObject * o;
  Probe * probe;
  ProbeMap * probeMap, * map1, * map2;
	ProbeMaker * probeMaker;
  char buffer[16384];

  initCollections();

  probeMaker = [ProbeMaker create: globalZone] ;

  o = [TestObject create: globalZone];

  printf("%s\n",[o name]) ;
	
  map1 = [probeMaker getProbeMap: [o class]] ;
  map2 = [probeMaker getProbeMap: [o class]] ;
	
  probe = [Probe createBegin: globalZone];
  [probe setProbedClass: [o class]];
  [probe setProbedVariable: "secondChar"];
  probe = [probe createEnd];

  probeMap = [ProbeMap createBegin: globalZone];
  [probeMap setProbedClass: [o class]];
  probeMap = [probeMap createEnd];

  o->firstInt = 1<<17 + 1;
  o->firstChar = 64;
  o->secondChar = 32;
  o->secondInt = - o->firstInt;
  o->longLong = 1<<31;
  o->longLong *= 64;

  printf("%s\n", [probe probeAsString: o Buffer: buffer]);
	[probe setStringReturnType: CharString] ;
	printf("%s\n", [probe probeAsString: o Buffer: buffer]);
	[probe setStringReturnType: IntString] ;
	printf("%s\n", [probe probeAsString: o Buffer: buffer]);
  printf("%s\n", [probeMap probeString: o Buffer: buffer]);
	printf("%s\n", [map1 probeString: o Buffer: buffer]);
	printf("%s\n", [map2 probeString: o Buffer: buffer]);

	exit(0) ;
}
