#import "Person.h"
#import <objectbase.h>
#import <objectbase/SwarmObject.h>
#import <space.h>
#import <activity.h>
#import <collections.h>
#import <random.h>
#import "SchellingWorld.h"


@interface Person: SwarmObject {
  int x, y;
  int xsize,ysize;
  int myColor;
  int unhappy;
  int nhoodType;
  int radius;
  int idnumber;
  double myTolerance;
  BOOL edgeWrap;
  SchellingWorld * myWorld;
}
	
- createEnd;

- setX: (int)inx Y: (int)iny;

- (int)getX;

- (int)getY;

- (void)setNhoodType: (int)nhood;

- (void)setNhoodRadius: (int)rad;

- (void)setEdgeWrap: (BOOL)wrap;

- setWorld: w;

- setColor: (int)c;
- (int)getColor;

- (void)setIDNumber: (int)numb;

- setTolerance: (double)t;
- (double)getTolerance;	

- step;

- moveToNewLocation;

- (double)verifyNhoodData: (int)t;

- (double)getFractionOf: (int)t ;

- drawSelfOn: (id)rast;
				
- (int)getUnhappy;


- (int)wrapXCoord: (int)inCoord;
- (int)wrapYCoord: (int)inCoord;
- (int)wrapCoord: (int)inCoord atModulus: (int)inModulus;
@end

