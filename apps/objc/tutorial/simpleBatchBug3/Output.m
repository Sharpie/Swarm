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
    
    fprintf (dataOutputFile, "currentTime\t avgEaten\n");	//Create headings for the file.
    dataFileExists = YES;	//Let everyone know that the datafile does exist, so we don't
    						//do this again.
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
  fprintf(  dataOutputFile, "%10ld \t %2.6f \n", \
	   		t, 									 \
	   		[eatenSequence getCurrentValue] );
	   		
  fflush( dataOutputFile ); //Forces all buffered data to be written to the file.
 
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



