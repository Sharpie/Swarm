#import <objectbase.h>

#import "Parameters.h"

#import <misc.h>  //This is a "multipurpose" include from the swarm library.  
                 // It contains header info for C functions atoi, strtod


//These are "convenience" functions that other objects can use to get
//values out of the Parameters object.
id
makeProbe (id obj, const char *ivarName)
{
  id probe = [VarProbe createBegin: [obj getZone]];
  [probe setProbedClass: [obj getClass]];
  [probe setProbedVariable: ivarName];
  return [probe createEnd];
}

int
getInt (id obj, const char *ivarName)
{
  id probe = makeProbe (obj, ivarName);
  int ret = [probe probeAsInt: obj];
  [probe drop];
  return ret;
}

@implementation Parameters

+ createBegin: aZone
 {
   static struct argp_option options[] = {
     {"inputfile", 'I',"filename",0,"set fn",5},
     {"run",'R',"RunNumber",0,"Run is...",6},
     {"randomSeed",'S',"SeedNumber",0,"Seed",7},
     {"bugDensity",'d',"D", 0, "Bug density", 8},
     {"seedProb",'p',"P",0,"Probability of Food",9},
     {"worldXSize",'x',"X",0,"Size of X dimension",10},
     {"worldYSize",'y',"Y",0,"Size of Y dimension",11},
     {"experimentDuration",'e',"E",0,"Duration of experiment",12},
     { 0 }
   };

   Parameters *obj = [super createBegin: aZone];
   [obj addOptions: options];

  
  obj->worldXSize = 80;
  obj->worldYSize = 80;
  obj->seedProb   = 0.5;
  obj->bugDensity = 0.1;
  obj->experimentDuration = 1000;
  obj->run= 01;

  return obj;
 }

- init
{
  id probeMap;
  probeMap = [EmptyProbeMap createBegin: [self getZone]];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];
  //This stuff is taken from the ModelSwarm.m file. 
  // Add in a bunch of variables, one per simulation parameter

  [probeMap addProbe: [probeLibrary getProbeForVariable: "worldXSize"
                                    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "worldYSize"
                                    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "seedProb"
                                    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "bugDensity"
                                    inClass: [self class]]];

  [probeMap addProbe: [probeLibrary getProbeForVariable: "experimentDuration"
                                    inClass: [self class]]];


  // Now install our custom probeMap into the probeLibrary.

  [probeLibrary setProbeMap: probeMap For: [self class]];
  return self;
}



- (int)parseKey: (int) key arg: (const char*) arg
{
  //We could just make a list of if statements that checks the argument
  // to see if it is equal to our parameters, like so:
  //   if (key == 'd')
  //       {
  //         if(arg)
  //  	     bugDensity = strtod (arg,NULL); 
  //         return 0;
  //       }
  //Doing that gets a bit tedious, so we use the switch statement from C:
  switch ( key )
    {
    case  'd' :
      if (arg)
	bugDensity = strtod (arg,NULL);  //strtod is "convert string to double" from stdlib
      break;
    
    case 'p' :
      if (arg)
	seedProb = strtod (arg,NULL);
      break;

    case 'x' :
      if (arg)
	worldXSize = atoi(arg);   //atoi--ascii to integer from stdlib
      break;
     
    case  'y' :
     
      if (arg)
        worldYSize = atoi(arg);
      break;
    
    case 'S' :
      if (arg)
	randomSeed = atoi (arg);
      break;
     
    case 'e' :
      if (arg)
	experimentDuration = atoi (arg);
      break;
    
    case 'I' :
      printf("here is an I. You might need it someday \n");
      break;
     
    case  'R' :
      if (arg)
	run = atoi(arg);
      break;
     
    default : 
      return [super parseKey: key arg: arg];
    }
  return 0;
}

- (double)getSeedProb
{
  return seedProb;
}

- (double)getBugDensity
{
  return bugDensity;
}

@end



