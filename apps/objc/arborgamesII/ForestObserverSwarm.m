#import "ForestObserverSwarm.h"
#import <collections.h>
#import <analysis.h>
#import <objectbase.h>
#import <gui.h>
#import "Forest.h"

@implementation ForestObserverSwarm

+ createBegin: aZone
{
  ForestObserverSwarm *obj;
  id <ProbeMap> probeMap;
  
  obj = [super createBegin: aZone];

  obj->displayFrequency = 1;
  obj->showSeedDistribution = 0;
  obj->showPopulationGraph = 1;

  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  [probeMap addProbe: [probeLibrary getProbeForVariable: "displayFrequency"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "showSeedDistribution"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "showPopulationGraph"
				    inClass: [self class]]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "showAgeLevel"
                             inClass: [self class]]
                        setHideResult: 1]];
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "showSpecies"
                             inClass: [self class]]
                        setHideResult: 1]];

  [probeLibrary setProbeMap: probeMap For: [self class]];

  return obj;
}

- createEnd
{
  return [super createEnd];
}

- _fireRasterDeath_ : caller
{
  [fireRaster drop];
  fireRaster = nil;
  return self;
}

- _forestRasterDeath_: caller
{
  [forestRaster drop];
  forestRaster = nil;
  return self;
}

- _youngForestRasterDeath_: caller
{
  [youngForestRaster drop];
  youngForestRaster = nil;
  return self;
}

- buildObjects
{
  id modelZone;
  int i;

  
  [super buildObjects];
  
  modelZone = [Zone create: [self getZone]];

  forestModelSwarm = [ForestModelSwarm create: modelZone];
 
  CREATE_ARCHIVED_PROBE_DISPLAY (forestModelSwarm);
  CREATE_ARCHIVED_PROBE_DISPLAY (self);
  
  [actionCache waitForControlEvent];
  if ([controlPanel getState] == ControlStateQuit)
    return self;

  [forestModelSwarm buildObjects];
  speciesNumber = [forestModelSwarm getSpeciesNumber];
  speciesList = [forestModelSwarm getSpeciesList];
 
  colormap = [Colormap create: [self getZone]];

  // We assume that there are less than 19 species...
  for (i = 0; i < speciesNumber; i++)
    {
      id aSpecies;
      
      // The color is also used as the species identifier with 0 being 
      // background. This is used when calling -getSpeciesIdentifier in
      // the case where we file snapshots of the forest (see ForestBatchSwarm
      // for details)...
      aSpecies = [speciesList atOffset: i];
      [colormap setColor: i + 1
                ToName: [aSpecies getColorName]];
      [aSpecies setColorMapEntry: i + 1];
      
    }
  
  if (showSeedDistribution)
    {
      for (i = 0; i < 100; i++)
        [colormap setColor: 31 + i ToRed: 0 
                  Green: 0.5 + (0.5*(((double)i) / 100.0))
                  Blue: 0];
      [colormap setColor: 30 ToName: "black"];
    }

  // We assume that there are less than 19 species...
  [colormap setColor: 21 ToName: "red"]; // The color of fire...
  [colormap setColor: 20 ToName: "black"]; // The color of non-fire...

  [colormap setColor: 25 ToName: "white"];
  [colormap setColor: 26 ToName: "LightCyan1"];
  [colormap setColor: 27 ToName: "PaleTurquoise"];
  [colormap setColor: 28 ToName: "Turquoise1"];
  [colormap setColor: 29 ToName: "Turquoise3"];
  
  forestRaster = [ZoomRaster createBegin: [self getZone]];
  SET_WINDOW_GEOMETRY_RECORD_NAME (forestRaster);
  forestRaster = [forestRaster createEnd];

  [forestRaster enableDestroyNotification: self
                notificationMethod: @selector(_forestRasterDeath_:)];

  [forestRaster setColormap: colormap];
  [forestRaster setZoomFactor: 4];
  [forestRaster setWidth: [forestModelSwarm getWorldSize] 
                  Height: [forestModelSwarm getWorldSize]];
  [forestRaster setWindowTitle: "The Forest"];
  [forestRaster pack];

  youngForestRaster = [ZoomRaster createBegin: [self getZone]];
  SET_WINDOW_GEOMETRY_RECORD_NAME (youngForestRaster);
  youngForestRaster = [youngForestRaster createEnd];

  [youngForestRaster enableDestroyNotification: self
                     notificationMethod: @selector(_youngForestRasterDeath_:)];

  [youngForestRaster setColormap: colormap];
  [youngForestRaster setZoomFactor: 4];
  [youngForestRaster setWidth: [forestModelSwarm getWorldSize] 
                  Height: [forestModelSwarm getWorldSize]];
  [youngForestRaster setWindowTitle: "The Next Generation"];
  [youngForestRaster pack];

  fireRaster = [ZoomRaster createBegin: [self getZone]];
  SET_WINDOW_GEOMETRY_RECORD_NAME (fireRaster);
  fireRaster = [fireRaster createEnd];

  [fireRaster enableDestroyNotification: self
              notificationMethod: @selector(_fireRasterDeath_:)];
  
  [fireRaster setColormap: colormap];
  [fireRaster setZoomFactor: 4];
  [fireRaster setWidth: [forestModelSwarm getWorldSize] 
                  Height: [forestModelSwarm getWorldSize]];
  [fireRaster setWindowTitle: "WildFires"];  
  [fireRaster pack];

  if (showSeedDistribution)
    {
      rasterList = [List create: [self getZone]];
      displayList = [List create: [self getZone]];
      for(i = 0; i < speciesNumber; i++){
        id aRaster;
        id aDisplay;
        id aSpecies;
        
        aSpecies = [speciesList atOffset: i];
        
        aRaster = [ZoomRaster create: [self getZone]];
        [aRaster setColormap: colormap];
        [aRaster setZoomFactor: 2];
        [aRaster setWidth: [forestModelSwarm getWorldSize] 
                 Height: [forestModelSwarm getWorldSize]];
        [aRaster setWindowTitle: [aSpecies getSpeciesName]];
        [aRaster pack];
        
        [rasterList addLast: aRaster];
        
        aDisplay = [Value2dDisplay createBegin: [self getZone]];
        [aDisplay setDisplayWidget: aRaster colormap: colormap];
        [aDisplay setDiscrete2dToDisplay:
                    [aSpecies getSeedSpace]]; 
        [aDisplay setDisplayMappingM: 1 C: 30];
        aDisplay = [aDisplay createEnd];
        
        [displayList addLast: aDisplay];
      }
    }
  
  treeDisplay = [Object2dDisplay createBegin: [self getZone]];
  [treeDisplay setDisplayWidget: forestRaster];
  [treeDisplay setDiscrete2dToDisplay: 
                 [[forestModelSwarm getTheForest] getTreeGrid: 1]];
#ifdef USELISTS 
  [treeDisplay setObjectCollection:
                 [[forestModelSwarm getTheForest] getTreeList: 1]]; 
#endif
  [treeDisplay setDisplayMessage: M(drawSelfOn:)];
  treeDisplay = [treeDisplay createEnd];

  youngForestDisplay = [Object2dDisplay createBegin: [self getZone]];
  [youngForestDisplay setDisplayWidget: youngForestRaster];
  [youngForestDisplay setDiscrete2dToDisplay: 
                 [[forestModelSwarm getTheForest] getTreeGrid: 0]]; 
#ifdef USELISTS
  [youngForestDisplay setObjectCollection:
                 [[forestModelSwarm getTheForest] getTreeList: 0]]; 
#endif
  [youngForestDisplay setDisplayMessage: M(drawSelfOn:)];
  youngForestDisplay = [youngForestDisplay createEnd];

  fireDisplay = [Value2dDisplay createBegin: [self getZone]];
  [fireDisplay setDisplayWidget: fireRaster colormap: colormap];
  [fireDisplay setDiscrete2dToDisplay: [forestModelSwarm getTheFireGrid]]; 
  [fireDisplay setDisplayMappingM: 1 C: 20]; // 20/21 should be black/red!!!
  fireDisplay = [fireDisplay createEnd];

  [forestRaster setButton: ButtonRight 
                Client: treeDisplay 
                Message: M(makeProbeAtX:Y:)];
  
  [youngForestRaster setButton: ButtonRight 
                     Client: youngForestDisplay 
                     Message: M(makeProbeAtX:Y:)];
  
  speciesGraph = [EZGraph createBegin: [self getZone]];
  SET_WINDOW_GEOMETRY_RECORD_NAME (speciesGraph);
  [speciesGraph setTitle: "number of species"];
  [speciesGraph setAxisLabelsX: "time" Y: "species"];
  speciesGraph = [speciesGraph createEnd];

  [speciesGraph createTotalSequence: "count"
                  withFeedFrom: speciesList
                   andSelector: M(stillActive)];

  entropyGraph = [EZGraph createBegin: [self getZone]];
  SET_WINDOW_GEOMETRY_RECORD_NAME (entropyGraph);
  [entropyGraph setTitle: "Species Entropy"];
  [entropyGraph setAxisLabelsX: "time" Y: "prop. of Max. Entropy"];
  entropyGraph = [entropyGraph createEnd];

  speciesEntropy = [Entropy createBegin: [self getZone]];
  [speciesEntropy setCollection: speciesList];
  [speciesEntropy setProbedSelector: M(getRelativeProportion)];
  speciesEntropy = [speciesEntropy createEnd];

  [entropyGraph createSequence: "H(species)"
                 withFeedFrom: speciesEntropy
                  andSelector: M(getEntropy)];

  if (showPopulationGraph)
    {

      const char *colors[speciesNumber];
  
      for (i = 0;i < speciesNumber; i++)
	{
	  colors[i] = strdup ( [[speciesList atOffset: i] getColorName] );
	
	  // Set colors for EZGraphs
	  fprintf(stderr,"Color %s \n", colors[i]);
	}
      popGraph = [EZGraph createBegin: [self getZone]];
      SET_WINDOW_GEOMETRY_RECORD_NAME (popGraph);
      [popGraph setTitle: "population"];
      [popGraph setAxisLabelsX: "time" Y: "population"];

      [popGraph setColors: colors count: speciesNumber];

      popGraph = [popGraph createEnd];
      
      for (i = 0;i < speciesNumber; i++)
        {
          id aSpecies;
          
          aSpecies = [speciesList atOffset: i];

          [popGraph createSequence: [aSpecies getSpeciesName] 
                    withFeedFrom: aSpecies 
                    andSelector: M(getCount)];
        }
    }  
  
  return self;
}  

- _eraseRasters_
{
  if (forestRaster)
    [forestRaster erase];
  if (youngForestRaster)
    [youngForestRaster erase];
  return self;
}

- _display_
{
  if (fireRaster)
    [fireDisplay display];
  if (forestRaster)
    [treeDisplay display];
  if (youngForestRaster)
    [youngForestDisplay display];
  return self;
}

- _drawRasters_
{
  if (fireRaster)
    {

#ifdef FIRE_SCREENSHOT
      MAKE_RASTER_SCREENSHOT(fireRaster);
#endif
      [fireRaster drawSelf];
    }
  if (forestRaster)
    {
#ifdef FOREST_SCREENSHOT
      MAKE_RASTER_SCREENSHOT(forestRaster);
#endif
      [forestRaster drawSelf];
    }
  if (youngForestRaster)
    {      
#ifdef YOUNG_FOREST_SCREENSHOT
      MAKE_RASTER_SCREENSHOT(youngForestRaster);
#endif
      [youngForestRaster drawSelf];
    }
  return self;
} 

- _drawGraphs_
{
  if (showPopulationGraph)
    {
#ifdef POPGRAPH_SCREENSHOT      
      MAKE_EZGRAPH_SCREENSHOT(popGraph);
#endif
      [popGraph step];
    }
#ifdef SPECIESGRAPH_SCREENSHOT        
  MAKE_EZGRAPH_SCREENSHOT(speciesGraph);
#endif
  [speciesGraph step];

  [speciesEntropy update];
#ifdef ENTROPYGRAPH_SCREENSHOT        
  MAKE_EZGRAPH_SCREENSHOT(entropyGraph);
#endif
  [entropyGraph step];

  return self;
}

- buildActions 
{
  int i;

  [super buildActions];
  
  [forestModelSwarm buildActions];
  
  displayActions = [ActionGroup create: [self getZone]];

  [displayActions createActionTo: self message: M(_eraseRasters_)];

  if (showSeedDistribution)
    for (i = 0; i < speciesNumber; i++)
      [displayActions createActionTo: [displayList atOffset: i]
                      message: M(display)];

  [displayActions createActionTo: self message: M(_display_)];

  if (showSeedDistribution)
    for (i = 0; i < speciesNumber; i++)
      [displayActions createActionTo: [rasterList atOffset: i]
                      message: M(drawSelf)];

  [displayActions createActionTo: self message: M(_drawRasters_)];

  [displayActions createActionTo: self message: M(_drawGraphs_)];

  //  if (showPopulationGraph)
  //      [displayActions createActionTo: popGraph message: M(step)];
  
  //    [displayActions createActionTo: speciesGraph message: M(step)];
  
  //    [displayActions createActionTo: speciesEntropy message: M(update)];
  //    [displayActions createActionTo: entropyGraph message: M(step)];

  [displayActions createActionTo: probeDisplayManager message: M(update)];
  [displayActions createActionTo: actionCache message: M(doTkEvents)];

  displaySchedule = [Schedule createBegin: [self getZone]];
  [displaySchedule setRepeatInterval: displayFrequency];
  displaySchedule = [displaySchedule createEnd];
  [displaySchedule at: 0 createAction: displayActions];
  
  return self;
}  

- activateIn: swarmContext
{
  [super activateIn: swarmContext];

  [forestModelSwarm activateIn: self];

  [displaySchedule activateIn: self];
  
  return [self getSwarmActivity];
}

- showAgeLevel
{
  [speciesList forEach: M(showAgeLevel)];
  return self;
}

- showSpecies
{
  [speciesList forEach: M(showSpecies)];
  return self;
}

@end

