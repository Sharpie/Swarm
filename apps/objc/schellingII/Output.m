#import "Output.h"

#include <misc.h> // stdio, time
#import "Person.h"
#import "ModelSwarm.h"
#import "Parameters.h"
#import "SchellingWorld.h"  //get object grid
#import "BatchColormap.h"
#import "BatchRaster.h"
#import "Parameters.h" // for run number
#import <analysis.h>
#import <gui.h> //ZoomRaster
#import <space.h> //setDiscrete2dToDisplay

#import <gui.h> //fillRectangle

@implementation Output


//Create things needed for writing data.  Make sure file names are set up.
- buildObjects
{
  dataFileExists = NO;

  [self createTimeString];

  [self createGraphWriter];
 
  [self prepareCOutputFile];

  [self createRaster];

  return self;
}

- (void)setAgentList: list
{
  agentList = list;
}

- (void)setModelSwarm: aSwarm
{
  modelSwarm= aSwarm;
}


- (void)createTimeString
{
  unsigned i;
  time_t runTime; /*"Return from the system's time() function"*/
  int runNumber = getInt((Parameters*)arguments,"run"); 
  
  if( runNumber == -1 )	//Run number is unspecified by user, we generate our own.
    {
      i = 0;
      
      runTime = time(NULL);					//Get the time in seconds.
      timeString = strdup( ctime(&runTime) );	//Creates a properly terminated string.
      
      //Normally we should check to make sure that timeString is not null,
      //but in this case we're going to play it a bit fast and loose...
      //REVIEW: Should this be the case? Should we use asserts more assertively (ha ha)?
      
      //this scans through and converts spaces and colons to underscores.
      
      while( timeString[ i ] != '\0' )
      	{
	  //First we strip those characters that we would prefer
	  //not to see in a unix operating system terminal...
	  if (timeString[i] == ' ' || timeString[i] == ':')
	    {
	      timeString[i] = '_';
	    } else if( timeString[i] == '\n' ) {	//Now we remove the end-of-line terminator.
	      timeString[i] = '\0';
	      break;								//Nothing left to see, we've shortened the string.
	    }
	  i++;	//Move one forward in timeString.
	}
    } else { //Run number was specified by the user.
      
      
      //So we need to create a string containing the run number that was specified.
      
      size_t actual_size;
      char buffer[ 3 ];	//This is a small buffer for the first few characters.
      
      //First we determine the size of the string to print.
      //This is actually something of a hack--it depends on the error message generated
      //by snprintf to give us the number of characters it actually needs, then we add
      //one to that so that there is room for the terminating character.  
      actual_size = snprintf( buffer, 3, "%d", runNumber ) + 1;
      
      //Now we allocate memory.   The sizeof( char ) is just a formality--it should be
      //"1" for any system.
      timeString = malloc( sizeof( char ) * actual_size );
      
      //snprintf copies (at most) actual_size - 1 characters into timeString and then,
      //if it can, trunicates the string with the string terminator '\0'.
      snprintf( timeString, actual_size, "%d", runNumber );
      
      /**
       ** For Linux, MacOS X, and just about everything else Swarm runs on except
       ** Solaris, there is a less convoluted way of doing this using a command
       ** called "asprintf."
       **
       ** asprintf mallocs memory to timeString and then prints runNumber into it
       ** using "%d" as a template.  Note the C function call--this is allowed in
       ** objective-c so long as its a C function that we're calling.
       **
       ** 	asprintf( &timeString, "%d", runNumber );
       **
       ** This does the same thing as the above five lines of code.
       **
       */
      
      
      
    } //if( runNumber == -1 ) - else
  
}

// Create an EZGraph object. This ALWAYS writes data into a file, if 
// you are either in GUI or batchmode. Only if you are in GUI mode
// will it draw a graph to the screen, however.
- (void)createGraphWriter
{
  char *hdfEZGraphName;
  id <HDF5> hdf5container; /*"HDF5 data container object used by bugGraph"*/

  //We'll use the snprintf trick above to store what we want the name of the graph to be.]
  size_t actual_size;
  char buffer[ 3 ];

  actual_size = snprintf( buffer, 3, "hdfGraph_%s.hdf", timeString) + 1;
  hdfEZGraphName = malloc( sizeof( char ) * actual_size );
  snprintf( hdfEZGraphName, actual_size, "hdfGraph_%s.hdf", timeString);

  hdf5container = [HDF5 createBegin: [self getZone]];
  [hdf5container setWriteFlag: YES];
  [hdf5container  setName: hdfEZGraphName];


  hdf5container = [hdf5container createEnd];
    
  moveGraph = [EZGraph createBegin: [self getZone]];
  [moveGraph setHDF5Container: hdf5container];
 
  [moveGraph setFileName: "movers"]; //not file name , but hdf5 object name

  if (swarmGUIMode == YES)
    {
      [moveGraph setGraphics: YES];
    }
  else
    {
      [moveGraph setGraphics: NO];
    }
  [moveGraph setFileOutput: YES]; 
  [moveGraph setTitle: "Fraction of people unhappy"];
  [moveGraph setAxisLabelsX: "time" Y: "proportion"];
  //??does this work:
  SET_WINDOW_GEOMETRY_RECORD_NAME (moveGraph);

  moveGraph = [moveGraph createEnd];
  
  moveSequence = [moveGraph createAverageSequence: "moved"
                     withFeedFrom: agentList
                     andSelector: M(getMoved)];

  unhappySequence = [moveGraph createAverageSequence: "unhappy"
		       withFeedFrom: agentList
		       andSelector: M(getUnhappy)];

}


- prepareCOutputFile
{
  char *outputFile;

  if (dataFileExists == YES) 
  {
  	return self;	//There already is a data file, so we really shouldn't
  					//do anything.
  } else {
  
	size_t actual_size;
	char buffer[ 3 ];
	
	actual_size = snprintf( buffer, 3, "output_%s.data", timeString ) + 1;
	outputFile = malloc( sizeof( char ) * actual_size );
  	snprintf( outputFile, actual_size, "output_%s.data", timeString );

  
    if( !( dataOutputFile = fopen( outputFile,"w" ) ) )	//Open/Create file.
    {
      abort();
    }
    
    fprintf (dataOutputFile, "currentTime\t unhappy\n");	//Create headings for the file.
    dataFileExists = YES;	//Let everyone know that the datafile does exist, so we don't
    						//do this again.
  }
  
  return self;
}


- (void)createRaster
{
  if (swarmGUIMode==1)
    {
      colormap = [Colormap create: [self getZone]];
      
      [colormap setColor: 0 ToName: "blue"];
      [colormap setColor: 1 ToName: "red"];
      [colormap setColor: 2 ToName: "green"];
      [colormap setColor: 3 ToName: "yellow"];
      [colormap setColor: 4 ToName: "purple"];
      [colormap setColor: 5 ToName: "cyan2"];
      [colormap setColor: 6 ToName: "LightSkyBlue3"];
      [colormap setColor: 7 ToName: "SlateGray4"];
      [colormap setColor: 8 ToName: "MidnightBlue"];
      [colormap setColor: 9 ToName: "DarkKhaki"];
      [colormap setColor: 10 ToName: "LightGoldenrod"];
      [colormap setColor: 11 ToName: "IndianRed"];
      [colormap setColor: 12 ToName: "DarkOrange"];
      [colormap setColor: 13 ToName: "HotPink"];
      [colormap setColor: 14 ToName: "SteelBlue2"];
      [colormap setColor: 15 ToName: "VioletRed"];
      [colormap setColor: 16 ToName: "turquoise4"];
      [colormap setColor: 17 ToName: "burlywood2"];
      [colormap setColor: 18 ToName: "gray70"];
      [colormap setColor: 19 ToName: "thistle1"];
      [colormap setColor: 21 ToName: "white"];
      
      worldRaster = [ZoomRaster createBegin: [self getZone]];
      SET_WINDOW_GEOMETRY_RECORD_NAME (worldRaster);
      worldRaster = [worldRaster createEnd];
      [worldRaster setColormap: colormap];
      [worldRaster setZoomFactor: 6];
      [worldRaster setWidth: [modelSwarm getWorldXSize] Height: [modelSwarm getWorldYSize]];
      [worldRaster setWindowTitle: "Schelling's world"];
      [worldRaster pack];
      
      worldDisplay = [Object2dDisplay createBegin: [self getZone] ];
      [worldDisplay setDisplayWidget: worldRaster];
      [worldDisplay setDiscrete2dToDisplay: [[modelSwarm getWorld] getObjectGrid]];
      [worldDisplay setObjectCollection: [modelSwarm getAgentList]];
      [worldDisplay setDisplayMessage: M(drawSelfOn:)];   
      worldDisplay = [worldDisplay createEnd];
      
      // This allows the user to right-click on the display to probe agents
      [worldRaster setButton: ButtonRight Client: worldDisplay Message: M(makeProbeAtX:Y:)];
    }
  else //we are a batch model, need raster output
    {
    
      colormap = [BatchColormap create: [self getZone]];
	
      [colormap setColor: 0 ToName: "blue"];
      [colormap setColor: 1 ToName: "red"];
      [colormap setColor: 2 ToName: "blue"];
      [colormap setColor: 3 ToName: "yellow"];
      [colormap setColor: 4 ToName: "purple"];
      [colormap setColor: 5 ToName: "cyan2"];
      [colormap setColor: 6 ToName: "LightSkyBlue3"];
      [colormap setColor: 7 ToName: "SlateGray4"];
      [colormap setColor: 8 ToName: "MidnightBlue"];
      [colormap setColor: 9 ToName: "DarkKhaki"];
      [colormap setColor: 10 ToName: "LightGoldenrod"];
      [colormap setColor: 11 ToName: "IndianRed"];
      [colormap setColor: 12 ToName: "DarkOrange"];
      [colormap setColor: 13 ToName: "HotPink"];
      [colormap setColor: 14 ToName: "SteelBlue2"];
      [colormap setColor: 15 ToName: "VioletRed"];
      [colormap setColor: 16 ToName: "turquoise4"];
      [colormap setColor: 17 ToName: "burlywood2"];
      [colormap setColor: 18 ToName: "gray70"];
      [colormap setColor: 19 ToName: "thistle1"];
      [colormap setColor: 21 ToName: "white"];
      
      worldRaster = [BatchRaster createBegin: [self getZone]];
      worldRaster = [worldRaster createEnd];
      [worldRaster setColormap: colormap];
      [worldRaster setWidth:[modelSwarm getWorldXSize] Height: [modelSwarm getWorldYSize]];
      [worldRaster pack];                           // draw the window.


      worldDisplay = [Object2dDisplay createBegin: [self getZone] ];
      [worldDisplay setDisplayWidget: worldRaster];
      [worldDisplay setDiscrete2dToDisplay: [[modelSwarm getWorld] getObjectGrid]];
      [worldDisplay setObjectCollection: [modelSwarm getAgentList]];
      [worldDisplay setDisplayMessage: M(drawSelfOn:)];   
      worldDisplay = [worldDisplay createEnd];
    }


}

- (void)updateRasterDisplay
{
  [self eraseRaster];
  [worldDisplay display];
  [worldRaster drawSelf];
  if (getInt(arguments,"writeGUIRaster"))
      [self writeGUIRaster];
}


- writeBatchRaster
{
  long currentTime  =  [(Parameters*)arguments getTime];

  //cant use standard erase method because it uses color 0, which I reserved
  //for blue agents. All the arrays in SchellingWorld use that value, so
  //it can't easily be changed.
  [worldRaster fillRectangleX0: 0 Y0: 0  X1:  ([modelSwarm getWorldXSize]-1) Y1: ([modelSwarm getWorldXSize]-1) Color: 21]; 

  [worldDisplay display];
  if (swarmGUIMode==0)
    {
      char filename[40];
      sprintf(filename, "R%d_t%05ld.ppm", getInt(arguments,"run"), currentTime);
      
      [(BatchRaster*)worldRaster  writeSelfToFile: filename];
    }
			
  return self;
}



- eraseRaster 
{
  // A method that overrides the default
  // black background for raster and paints
  // the raster white instead
  [worldRaster fillRectangleX0: 0
	                    Y0: 0
	                    X1: [modelSwarm getWorldXSize] 
			    Y1: [modelSwarm getWorldYSize]
	                 Color: 21]; 
	return self;

}

- writeGUIRaster
{
  char filename[40];
  sprintf (filename, "Raster_t%04ldRUN%07d.png", [(Parameters *)arguments getTime], getInt(arguments,"run"));
  id <Pixmap> apix = [Pixmap createBegin: [self getZone]];
  [apix setWidget: worldRaster];
  [apix setDecorationsFlag: NO];
  apix = [apix    createEnd];
  [apix save: filename];
  [apix drop];
  return self;
}




// This method dumps out measures into a text file
- writeCData
{
  long t = getCurrentTime();
  
  // First, just dump out the raw numbers in text.
  // This is the old standby!

  // The \t is for tab separated data
  fprintf(  dataOutputFile, "%10ld \t %2.6f \n", \
	   		t, 									 \
	   		[moveSequence getCurrentValue] );
	   		
  fflush( dataOutputFile ); //Forces all buffered data to be written to the file.
 
  return self;
}


- (void)step
{
  if (!moveGraph) [self createGraphWriter];
  [moveGraph step];
  [self writeCData];
	
  if (swarmGUIMode == 1)
    [self updateRasterDisplay];
  else
    [self writeBatchRaster];
}  







- (BOOL)checkToStop
{
  static int emptySteps;
  if ( [moveSequence getCurrentValue] < 0.0001 ) 
    {
      emptySteps++;
    }					   
  else
    {
      emptySteps = 0;
    }
  if (emptySteps > 10) return YES;
  return NO;
}





/*"It is necessary to drop the data writing objects in order to make
sure they finish their work.
"*/
- (void)drop
{
  if (dataOutputFile) fclose(dataOutputFile);
  if (moveGraph) [moveGraph drop];

  [self writeBatchRaster];


  [super drop];
}

@end



