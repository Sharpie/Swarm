#import <objectbase.h>
#import <simtools.h>  //for  Objectloader & Saver
#import "Parameters.h"

#import <misc.h>  //for atoi

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


double
getDbl (id obj, const char *ivarName)
{
  id probe = makeProbe (obj, ivarName);
  double ret = [probe probeAsDouble: obj];
  [probe drop];
  return ret;
}



@implementation Parameters

+ createBegin: aZone
 {
   static struct argp_option options[] = {
     {"inputfile", 'I',"inputFilename",0,"use parameters in file",5},
     {"run",'r',"RunNumber",0,"Run is...",6},
     {"worldXSize", 'x', "worldXSize",0,"worldXSize",7},
     {"worldYSize",'y',"worldYSize",0,"worldYSize",8},
     {"experimentDuration",'e',"experimentDuration",0,"experimentDuration",9},
     {"radius",'d',"radius",0,"radius of vision",10},
     {"fractionVacant",'f',"fractionVacant",0,"fraction vacant cells",11},
     {"fractionBlue",'B',"fractionBlue",0,"fraction blue",12},
     {"fractionRed",'R',"fractionRed",0,"fraction red",13},
     { 0 } 
   };
   
   Parameters *obj = [super createBegin: aZone];
   [obj addOptions: options];
   obj->worldXSize = 80;
   obj->worldYSize = 80;
   
   obj->numRaces = 2;
   obj->neighborhood_type = strdup("vonneuman");
   obj->radius = 1;
   obj->edgeWrap = YES;
   obj->synchronous = NO;
   obj->fractionVacant = 0.2;
   obj->fractionBlue = 0.5;
   obj->fractionRed = 0.5;
   obj->blueToleranceUpper=0.50;
   obj->blueToleranceLower=0.25;
   obj->redToleranceUpper=0.50;
   obj->redToleranceLower=0.25;
   obj->otherToleranceUpper=0.50;
   obj->otherToleranceLower=0.25;
   obj->randomize = NO;
   obj->experimentDuration = 1000;
   obj->currentTime = 0;
   obj->run= -1;
   obj->inputFilename = NULL;
   obj->writeGUIRaster = NO;
   return obj;
 }



- init
{
  id probeMap;
  probeMap = [EmptyProbeMap createBegin: [self getZone]];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];
  // Add in a bunch of variables, one per simulation parameter
   [probeMap addProbe: [probeLibrary getProbeForVariable: "worldXSize"
                                    inClass: [self class]]];
   [probeMap addProbe: [probeLibrary getProbeForVariable: "worldYSize"
                                    inClass: [self class]]];

   
  // Add in a bunch of probes one per simulation parameter
  [probeMap addProbe: [probeLibrary getProbeForVariable: "numRaces"
				  inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "neighborhood_type"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "radius"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "edgeWrap"
				     inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "synchronous"
				   inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "fractionVacant"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "fractionBlue"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "fractionRed"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "blueToleranceUpper"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "blueToleranceLower"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "redToleranceUpper"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "redToleranceLower"
				    inClass: [self class]]];

  [probeMap addProbe: [probeLibrary getProbeForVariable: "otherToleranceUpper"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "otherToleranceLower"
				    inClass: [self class]]];




  [probeMap addProbe: [probeLibrary getProbeForVariable: "randomSeed"
				    inClass: [self class]]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "saveParameters:"
			                        inClass: [self class]]
			                  setHideResult: 1]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "loadParameters:"
			                        inClass: [self class]]
			                  setHideResult: 1]];


  [probeMap addProbe: [probeLibrary getProbeForVariable: "experimentDuration"
                                    inClass: [self class]]];
  
  [probeMap addProbe: [probeLibrary getProbeForVariable: "run"
                                    inClass: [self class]]];


  

  [probeMap addProbe: [[probeLibrary getProbeForMessage: "toggleWriteGUIRaster"
				     inClass: [self class]]
			setHideResult: 0]];

  [probeMap addProbe: [[probeLibrary getProbeForMessage: "toggleEdgeWrap"
				     inClass: [self class]]
			setHideResult: 0]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "toggleRandomizeList"
				     inClass: [self class]]
			setHideResult: 0]];

  [probeMap addProbe: [[probeLibrary getProbeForMessage: "takeScreenshot"
				     inClass: [self class]]
			setHideResult: 0]];


  [probeLibrary setProbeMap: probeMap For: [self class]];
  

  if (inputFilename)
    [(Parameters *)self loadParameters: inputFilename];
    


  return self;
}



- (int)parseKey: (int) key arg: (const char*) arg
 {
   if (key == 'x')
     {
       if(arg)
	 worldXSize = atoi(arg);
       return 0;
     }
   else if (key == 'y')
     {
       if(arg)
	 worldYSize = atoi(arg);
       return 0;
     }

   else if (key == 'r')
     {
       if(arg)
	 run = atoi(arg);
       return 0;
     }
   else if (key == 'e')
     {

       if(arg)
	 experimentDuration = atoi(arg);
       return 0;
     }

   else if (key == 'v')
     {

       if(arg)
	 radius = atoi(arg);
       return 0;
     }
   else if (key == 'f')
     {
       if(arg)
	 fractionVacant = strtod(arg,0); 
       return 0;
     }
   else if (key == 'R')
     {
       if(arg)
	 fractionRed = strtod(arg,0); 
       return 0;
     }
   else if (key == 'B')
     {
       if(arg)
	 fractionBlue = strtod(arg,0); 
       return 0;
     }
   
   else if (key == 'I')
     {
       if (arg)
	 {
	   inputFilename = strdup(arg);
	 }
       return 0;
     }

   else
     return [super parseKey: key arg: arg];
 }


- (void)setCurrentTime: (long)x
{
  currentTime = x;
}


- (timeval_t)getTime
{
  return currentTime;
}


- (char *)getInputFilename
{
  if (inputFilename)
    return strdup(inputFilename);
  else
    return NULL;
}

- (char *)getNeighborhoodType
{
  return strdup(neighborhood_type);
}



- (BOOL)toggleRandomizeList
{
  if (randomize == YES) randomize = NO;
  else randomize = YES;
  return randomize;
}


- (BOOL)toggleEdgeWrap
{
  if (edgeWrap == YES) edgeWrap = NO;
  else edgeWrap = YES;
  return edgeWrap;
}

- (BOOL)toggleWriteGUIRaster
{
  if (writeGUIRaster == YES) writeGUIRaster = NO;
  else writeGUIRaster = YES;
  return writeGUIRaster;
}


- takeScreenshot
{
  char fn[40];
  sprintf (fn, "SShot_t%04ldRUN%07d.png", currentTime , run);
  id <Pixmap> apix = [Pixmap createBegin: [self getZone]];
  [apix setWidget: nil];
  apix = [apix    createEnd];
  [apix save: fn];
  [apix drop];
  return self;
}




// Methods associated with probe above, to 
// save or load the parameters from a file
- saveParameters: (char*)fn
{
  [ObjectSaver save: self toFileNamed: fn];
  
  return self;
}
  
- loadParameters: (char*)fn 
{
  [ObjectLoader load: self fromFileNamed: fn];
  return self;
}




@end










