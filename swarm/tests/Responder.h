/*
Name:           Responder.h
Description:    Responder is a simple class that keeps track of all of 
                calls to its methods.
Test suite:     activity
*/               

#import <objectbase/SwarmObject.h>

extern int messages[10];
extern int ids[10];
extern int rtimes[10];
extern void init_tables (void);

@interface Responder: SwarmObject
{
  int Id;
}

+ create: aZone withId: (int)id;
- setId: (int)id;
- (int)getId;
- m1;
- m2;
- m3;
- m4;
- m5;
- mId;
- mTimeId;

@end

