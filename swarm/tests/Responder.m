/*
Name:           Responder.m
Description:    Responder class implementation
Test suite:     activity
*/               


#import "Responder.h"
#import <activity.h>

/*
Messages array is used for keeping track of the order of invocation of 
m1..m5 messages. (This is a bit of a hack, but useful for diagnosis in 
tests, where ordering is being tested)
*/

int messages[10];

/*
Ids array is used for keeping track of different Responders in a collections
that which received mId message. (Again a hack, but useful in testing
ordering in collection related actions - ForEach).
*/
 
int ids[10];

/*
Rtimes array is used for keeping track of times when the mTimeId method was 
invoked. (Useful for Schedule testing)
*/

int rtimes[10];

/*
 Index variables for messages, ids and times arrays
*/

int morder,idorder, timer;

void init_tables(void)
{
  int i;
  for (i=0;i<10;i++)
    {
      messages[i]=0;
      ids[i] = 0;
      rtimes[i] = 0;
    }
  morder = 1;
  idorder = 1;
  timer = 0;
}

@implementation Responder

+ create: aZone withId: (int) id
{
  Responder *resp = [aZone allocIVars: self];
  resp->Id = id;
  return resp; 
}

- setId: (int) id
{
  Id = id;
  return self;
}

- (int) getId
{
  return Id;
}

- m1 
{
  printf("m1\n");
  fflush(stdout);
  messages[0] = morder++;
  return self;
}
- m2 
{
  printf("m2\n");
  fflush(stdout);
  messages[1] = morder++;
  return self;
}
- m3
{
  printf("m3\n");
  fflush(stdout);
  messages[2] = morder++;
    return self;
}
- m4 
{
  printf("m4\n");
  fflush(stdout);
  messages[3] = morder++;
  return self;
}
- m5 
{
  printf("m5\n");
  fflush(stdout);
  messages[4] = morder++;
  return self;
}

- mId 
{
  printf("%d\n", Id);
  fflush(stdout);
  ids[Id - 1] = idorder++;
  return self;
}

-mTimeId
{
  printf("%d: %d\n", (int)getCurrentTime(), Id);
  ids[timer] = Id;
  rtimes[timer++] = getCurrentTime();
  return self;
}

@end

