
#import <objectbase.h>
#import <objectbase/SwarmObject.h>
#import <simtools.h>
#import <analysis.h>
#import <space.h>

@interface Output: SwarmObject
{
  @private
    BOOL dataFileExists; /*"Indicator that dataFile initialization has alreadyoccurred"*/

  id modelSwarm;
  id agentList;
  
  char * timeString;/*"a verbose description of current time"*/
  FILE * dataOutputFile; /*"FILE handle for output from C style fprintf"*/

  id <EZAverageSequence> moveSequence;  
  id <EZAverageSequence> unhappySequence;
  id <EZGraph> moveGraph;
 /*"EZGraph object that is used only to create hdf5 formatted output"*/

 
  id <ZoomRaster> worldRaster;
  id <Object2dDisplay> worldDisplay;
}

- buildObjects;

- (void)setAgentList: list;
- (void)setModelSwarm: aSwarm;

- (void)createTimeString;

- prepareCOutputFile;
- eraseRaster;

-writeGUIRaster;
- writeCData;

- (void)step;

- (void)createGraphWriter;

- (void)createRaster;

- (void)updateRasterDisplay;
- writeBatchRaster;
- (BOOL)checkToStop;

- (void)drop;

@end




