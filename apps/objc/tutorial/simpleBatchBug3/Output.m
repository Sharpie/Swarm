#import "Output.h"

#include <misc.h> // stdio, time
//#import <time.h>
#import "Bug.h"

#import "Parameters.h"

@implementation Output


//Create things needed for writing data.  Make sure file names are set up.
- buildObjects
{
  dataFileExists = NO;

  [self createTimeString];

  [self createGraphWriter];
 
  [self prepareCOutputFile];

  return self;
}

- setBugList: list
{
  bugList = list;
  return self;
}


- (void)createTimeString
{
	int i;
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
      
      	asprintf( &timeString, "%4d", runNumber ); //Should we free these somewhere? a dealloc?
      	
    } //if( runNumber == -1 ) - else
  
}

// Create an EZGraph object. This ALWAYS writes data into a file, if 
// you are either in GUI or batchmode. Only if you are in GUI mode
// will it draw a graph to the screen, however.
- (void)createGraphWriter
{
  char hdfEZGraphName[100];
  id <HDF5> hdf5container; /*"HDF5 data container object used by bugGraph"*/
  strcpy (hdfEZGraphName,"hdfGraph");
  strcat (hdfEZGraphName, timeString);
  strcat (hdfEZGraphName, ".hdf");

  hdf5container = [HDF5 createBegin: [self getZone]];
  [hdf5container setWriteFlag: YES];
  [hdf5container  setName: hdfEZGraphName];
  hdf5container = [hdf5container createEnd];
    
  bugGraph = [EZGraph createBegin: [self getZone]];
  [bugGraph setHDF5Container: hdf5container];
 
  [bugGraph setFileName: "bugs"]; //not file name , but hdf5 object name

  if (swarmGUIMode == YES)
    {
      [bugGraph setGraphics: YES];
    }
  else
    {
      [bugGraph setGraphics: NO];
    }
  [bugGraph setFileOutput: YES]; 
  [bugGraph setTitle: "Bug Diet Report"];
  [bugGraph setAxisLabelsX: "time" Y: "proportion who ate"];
  bugGraph = [bugGraph createEnd];
  
  eatenSequence = [bugGraph createAverageSequence: "eaten"
		       withFeedFrom: bugList
		       andSelector: M(getHaveEaten)];

}


- prepareCOutputFile
{
  char outputFile[256];

  if (dataFileExists == YES) return self;

  else{
    snprintf (outputFile,256, "output%s.data",timeString);

    
    if(!(dataOutputFile = fopen(outputFile,"w")))
      abort();
    fprintf (dataOutputFile, "currentTime\t avgEaten\n");
    dataFileExists = YES;
  }
  return self;
}


// This method dumps out measures into a text file
- writeCData
{
  long t = getCurrentTime();
  // First, just dump out the raw numbers in text.
  // This is the old standby!

  // The \t is for tab separated data
  fprintf (dataOutputFile, "%10ld \t %2.6f \n", 
	   t, 
	   [eatenSequence getCurrentValue]
           );
  fflush ( dataOutputFile );
 
  return self;
}


- step
{
  if (!bugGraph) [self createGraphWriter];
  [bugGraph step];
  [self writeCData];
  return self;
}  


- (BOOL)checkToStop
{
  static int emptySteps;
  if ( [eatenSequence getCurrentValue] < 0.0001 ) 
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
  if (bugGraph) [bugGraph drop];
  
  [super drop];
}

@end



