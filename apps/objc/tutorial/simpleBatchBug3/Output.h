
#import <objectbase.h>
#import <objectbase/SwarmObject.h>
#import <simtools.h>
#import <analysis.h>

@interface Output: SwarmObject
{
  @private
    BOOL dataFileExists; /*"Indicator that dataFile initialization has alreadyoccurred"*/

  id bugList;
  
  char * timeString;/*"a verbose description of current time"*/
  FILE * dataOutputFile; /*"FILE handle for output from C style fprintf"*/

  id <EZAverageSequence> eatenSequence;  

  id <EZGraph> bugGraph; /*"EZGraph object that is used only to create hdf5 formatted output"*/

}

- buildObjects;

- setBugList: list;

- (void)createTimeString;

- prepareCOutputFile;

- writeCData;

- step;

- (void)createGraphWriter;

- (BOOL)checkToStop;

- (void)drop;

@end




