// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase.h>
#import <objectbase/Probe.h>
#import <objectbase/VarProbe.h>
#import <objectbase/ProbeMap.h>
#import <collections.h>
#import <simtools.h>

@interface TestObject: SwarmObject
{
  int firstInt;
  long long longLong;
  char firstChar;
  char secondChar;
  int secondInt;
  float aFloat;
}
@end

@implementation TestObject
@end

int
main (int argc, const char ** argv)
{
  TestObject * o;
  VarProbe *probe;
  VarProbe *probe2;
  ProbeMap * probeMap1; 
  ProbeMap * probeMap2; 
  char buffer[16384];
  char buffer2[16384];

  initSwarmBatch(argc, argv);

  o = [TestObject create: globalZone];

  printf("Using %s\n",[o name]) ;
  
  probe = [VarProbe createBegin: globalZone];
  [probe setProbedClass: [o class]];
  [probe setProbedVariable: "secondChar"];
  probe = [probe createEnd];

  probe2 = [VarProbe createBegin: globalZone];
  [probe2 setProbedClass: [o class]];
  [probe2 setProbedVariable: "aFloat"];   
  probe2 = [probe2 createEnd];
  [probe2 setFloatFormat: "%.3f"];
  

  o->firstInt = (1<<17) + 1;
  o->firstChar = 64;
  o->secondChar = 32;   // the printable version of this is a space
  o->secondInt = - o->firstInt;
  o->longLong = 1<<31;
  o->longLong *= 64;
  o->aFloat = 0.675264652452;

  // test setStringReturnType setting methods

  printf("first probe\n");
  printf("%s\n", [probe probeAsString: o Buffer: buffer]);
  if (strcmp(buffer, "32 ' '") != 0)
    {
      fprintf(stderr, "Default probe on char invalid \n");
      return 1;
    }  
    
  [probe setStringReturnType: CharString] ;
  printf("%s\n", [probe probeAsString: o Buffer: buffer]);
  if (strcmp(buffer, "' '") != 0)
    {
      fprintf(stderr, "Char probe on char invalid \n");
      return 1;
    }  
  
  [probe setStringReturnType: IntString] ;

  printf("%s\n", [probe probeAsString: o Buffer: buffer]);
  if (strcmp(buffer, "32") != 0)
    {
      fprintf(stderr, "Int probe on char invalid \n");
      return 1;
    }  

  // test access to precision probing directly via VarProbes

  printf("second probe\n");
  printf("%s\n", [probe2 probeAsString: o Buffer: buffer2]);

  if (strcmp(buffer2, "0.675") != 0)
    {
      fprintf(stderr, "Probe on float not returning correct precision\n");
      return 1;
    }

  // test access to precision probing via ProbeMaps

  probeMap1 = [EmptyProbeMap createBegin: globalZone];
  [probeMap1 setProbedClass: [o class]];
  probeMap1 = [probeMap1 createEnd];

  printf("first ProbeMap\n");
  [probeMap1 addProbe: 
               [probeLibrary getProbeForVariable: "aFloat"
                             inClass: [o class]]];
  
  [probeLibrary setProbeMap: probeMap1 For: [o class]];

  printf("%s\n", [[probeLibrary getProbeForVariable: "aFloat" inClass: [o class]]
                   probeAsString: o Buffer: buffer]);
  
  if (strcmp(buffer, "0.675") == 0)
    {
      fprintf(stderr, "Probe on float installed in global `probeLibrary' not returning enough precision \n");
      return 1;
    }

  // override probeMap1 by probeMap2 in probeLibrary for `TestObject' class

  printf("second ProbeMap\n");

  probeMap2 = [EmptyProbeMap createBegin: globalZone];
  [probeMap2 setProbedClass: [o class]];
  probeMap2 = [probeMap2 createEnd];

  [probeMap2 addProbe: 
              [[probeLibrary getProbeForVariable: "aFloat"
                             inClass: [o class]] setFloatFormat: "%.3f"]];
  
  [probeLibrary setProbeMap: probeMap2 For: [o class]];
  
  printf("%s\n", [[probeLibrary getProbeForVariable: "aFloat" inClass: [o class]]
                   probeAsString: o Buffer: buffer]);
  
  if (strcmp(buffer, "0.675") != 0)
    {
      fprintf(stderr, "Probe on float installed in global `probeLibrary' instance not returning 3 sig fig precision\n");
      return 1;
    }
  
  return 0;
}


