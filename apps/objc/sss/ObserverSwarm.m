// Sugarscape in Swarm. Copyright © 1997-1998 Nelson Minar
// This program is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "ObserverSwarm.h"
#import <gui.h> //Pixmap


@implementation ObserverSwarm

// Set up configurable display parameters
//   how often to update the display,
+ createBegin: aZone
{
  ObserverSwarm *obj;
  id <ProbeMap> probeMap;
  
  obj = [super createBegin: aZone];

  obj->displayFrequency = 1;
  obj->drawPopulationGraph = 1;
  obj->drawWealthHistogram = 1;
  obj->parameterFile = NULL;

  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  [probeMap addProbe: [probeLibrary getProbeForMessage: "setParameterFile:"
 				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForMessage: "saveParameters:"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "drawPopulationGraph"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "drawWealthHistogram"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "displayFrequency"
				    inClass: [self class]]];

  [probeLibrary setProbeMap: probeMap For: [self class]];

  return obj;
}

- _wealthHistogramDeath_: caller
{
  [wealthHistogram drop];
  wealthHistogram = nil;
  return self;
}

- _worldRasterDeath_: caller
{
  [worldRaster drop];
  worldRaster = nil;
  return self;
}

// Create all the objects.
- buildObjects
{
  int i;
  int maxSugarValue;
  SugarSpace *sugarSpace;
  id agentList;

  [super buildObjects];
  
  // Show a probe map so that users can type in a parameter
  // file name if they want. It is ARCHIVED, meaning it will
  // save the window positions if you hit the "save" button.
  CREATE_ARCHIVED_PROBE_DISPLAY (self);

 // Set our state to stopped - this code also waits until the user
  // clicks "go" so he has a chance to change parameters.
  [controlPanel setStateStopped];


  // If a parameter file was entered, use it. 
  if (parameterFile != NULL)
    {
      id archiver = [LispArchiver create: self setPath: parameterFile];
      

      if ((modelSwarm = [archiver getWithZone: self
                                            key: "model"]) == nil)
	raiseEvent(InvalidOperation,
		   "Can't find the parameters to create modelSwarm");
      [archiver drop];
    }
  else
    {
      modelSwarm = [ModelSwarm create: self];
    }
  // Now create probes for the Swarms so the user can retool settings
  CREATE_ARCHIVED_PROBE_DISPLAY (modelSwarm);
  
  // Stop the control panel again in case users want to revise
  // the settings.
  [controlPanel setStateStopped];

  
  // Now tell the model swarm to build its objects.
  [modelSwarm buildObjects];

 
  // Read some handy objects out of the model swarm
  sugarSpace = [modelSwarm getSugarSpace];
  agentList = [modelSwarm getAgentList];

  // Compute the colourmap
  maxSugarValue = [[modelSwarm getSugarSpace] getGlobalMaxSugar];

  fprintf(stderr,"MaxSugarValue %d \n," , maxSugarValue);

  colormap = [Colormap create: self];
  // Colours - shades of yellow for sugar values
  for (i = 0; i < maxSugarValue; i++)
    [colormap setColor: i
  	      ToRed: (double) i / (maxSugarValue - 1.0)
  	      Green: (double) i / (maxSugarValue - 1.0)
  	      Blue: 0];
 

  // Red for agents. Assigned to the number 100 - the agent needs to
  // know this magic number.
  [colormap setColor: 100 ToName: "red"];	  // agent colour

  // Next, create a 2d window for displaying the world.
  worldRaster = [ZoomRaster createBegin: self];
  SET_WINDOW_GEOMETRY_RECORD_NAME (worldRaster);
  worldRaster = [worldRaster createEnd];
  [worldRaster enableDestroyNotification: self
               notificationMethod: @selector (_worldRasterDeath_:)];
  [worldRaster setColormap: colormap];
  [worldRaster setZoomFactor: 6];
  [worldRaster setWidth: [[sugarSpace getAgentGrid] getSizeX]
               Height: [[sugarSpace getAgentGrid] getSizeY]];
  [worldRaster setWindowTitle: "SugarScape"];
  [worldRaster pack];                             // draw the window.

  // Create an object to display the sugar values
  sugarDisplay = [Value2dDisplay createBegin: self];
  [sugarDisplay setDisplayWidget: worldRaster colormap: colormap];
  [sugarDisplay setDiscrete2dToDisplay: [[modelSwarm getSugarSpace] getSugarValues]];
  // [sugarDisplay setDisplayMappingM: 1 C: 0];	  // map to colourmap
  sugarDisplay = [sugarDisplay createEnd];

  // And an object to display the agents
  agentDisplay = [Object2dDisplay createBegin: self];
  [agentDisplay setDisplayWidget: worldRaster];
  [agentDisplay setDiscrete2dToDisplay: [sugarSpace getAgentGrid]];
  [agentDisplay setObjectCollection: [modelSwarm getAgentList]];
  [agentDisplay setDisplayMessage: M(drawSelfOn:)];   // draw method
  agentDisplay = [agentDisplay createEnd];

  // Enable probes on the world.
  [worldRaster setButton: ButtonRight
               Client: agentDisplay
               Message: M(makeProbeAtX:Y:)];

  if (drawPopulationGraph)
    {
      // And create a graph of population in the world
      populationGraph = [EZGraph createBegin: self];
      SET_WINDOW_GEOMETRY_RECORD_NAME (populationGraph);
      [populationGraph setTitle: "Population over time"];
      [populationGraph setAxisLabelsX: "time" Y: "population"];
      populationGraph = [populationGraph createEnd];
      
      // One data sequence in the graph - total population
      [populationGraph createSequence: "population"
                       withFeedFrom: agentList
                       andSelector: M(getCount)];
    }
  
  // Create a graph for various agent attributes
  attributeGraph = [EZGraph createBegin: self];
  SET_WINDOW_GEOMETRY_RECORD_NAME (attributeGraph);
  [attributeGraph setTitle: "Agent attributes over time"];
  [attributeGraph setAxisLabelsX: "time" Y: "attribute"];
  attributeGraph = [attributeGraph createEnd];
  
  // Two data sequences here. Average vision for all the bugs
  [attributeGraph createAverageSequence: "vision"
		  withFeedFrom: agentList
		  andSelector: M(getVision)];
  // And average metabolism
  [attributeGraph createAverageSequence: "metabolism"
		  withFeedFrom: agentList
		  andSelector: M(getMetabolism)];

  if (drawWealthHistogram)
    {
      // Create a histogram of agent wealth distribution
      wealthHistogram = [EZBin createBegin: self];
      SET_WINDOW_GEOMETRY_RECORD_NAME (wealthHistogram);
      [wealthHistogram setTitle: "Agent wealth distribution"];
      [wealthHistogram setAxisLabelsX: "wealth" Y: "number of agents"];
      [wealthHistogram setBinCount: 9];
      [wealthHistogram setLowerBound: 0];
      [wealthHistogram setUpperBound: 300];
      [wealthHistogram setCollection: agentList];
      [wealthHistogram setProbedSelector: M(getCurrentSugar)];
      wealthHistogram = [wealthHistogram createEnd];
      [wealthHistogram enableDestroyNotification: self
                       notificationMethod: @selector(_wealthHistogramDeath_:)];
    }
  
  return self;
}  

- _updateHistogram_
{
  if (wealthHistogram)
    {
      [wealthHistogram reset];
      [wealthHistogram update];
      [wealthHistogram output];
    }
  return self;
}

- _updateDisplay_
{
  if (worldRaster)
    {
      [sugarDisplay display];
      [agentDisplay display];
      [worldRaster drawSelf];
    }
  return self;
}

- (char*)setParameterFile: (char*)aString
{
  char temp[100];
  sprintf (temp,"parameters/%s.scm",aString); 
  parameterFile = strdup (temp);
  return parameterFile;
}

      
- saveParameters: (char*)aString
 {
   char dataArchiveName[100];
   snprintf(dataArchiveName,100,"parameters/%s.scm",aString);
   id dataArchiver = [LispArchiver create: self setPath: dataArchiveName];
   
   [dataArchiver putShallow: "model" object: modelSwarm];
   [dataArchiver sync];
   [dataArchiver drop];
   return self;
 }



// The display schedule
- buildActions 
{
  [super buildActions];
  
  [modelSwarm buildActions];

  // The display schedule is just a list of actions in a row
  //   display the sugar
  //   display the agents
  //   update the display itself
  //   update the graphs
  //   update probes
  //   update control panel events
  displayActions = [ActionGroup create: self];
  [displayActions createActionTo: self message: M(_updateDisplay_)];

  [displayActions createActionTo: attributeGraph   message: M(step)];
  if (drawPopulationGraph)
    [displayActions createActionTo: populationGraph message: M(step)];
  if (drawWealthHistogram)
    [displayActions createActionTo: self message: M(_updateHistogram_)];

  [displayActions createActionTo: probeDisplayManager  message: M(update)];
  [displayActions createActionTo: actionCache          message: M(doTkEvents)];
#ifdef MAKEMOVIE
  [displayActions createActionTo: self                 message: M(writeFrame)];
#endif
  
  displaySchedule = [Schedule createBegin: self];
  [displaySchedule setRepeatInterval: displayFrequency];
  displaySchedule = [displaySchedule createEnd];
  [displaySchedule at: 0 createAction: displayActions];

  return self;
}  

// scheduling details. The model swarm is activated here.
- (id <Activity>)activateIn: swarmContext
{
  [super activateIn: swarmContext];
  [modelSwarm activateIn: self];
  [displaySchedule activateIn: self];
  return [self getActivity];
}

#ifdef MAKEMOVIE
- writeFrame
{
  char filename[256];
  id aPixmap;
  sprintf(filename, "%04ld.ppm", getCurrentTime());
  aPixmap = [Pixmap createBegin: self];
  [aPixmap setWidget: worldRaster];
  [aPixmap setDecorationsFlag: NO];
  aPixmap = [aPixmap createEnd];
  [aPixmap save: filename];
  [aPixmap drop];
  return self;
}
#endif

@end
