// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Symbol.h
Description:  classes to define distinct global id constants
Library:      defobj
*/

#import <defobj/Create.h>

@interface Symbol_c : CreateDrop_s
{
@public
  char *name;
}
/*** methods implemented in .m file ***/
+ create: aZone setName: (char *)symbolName;
- (char *) getName;
@end

@interface EventType_c : Symbol_c
/*** methods implemented in .m file ***/
- (void) raiseEvent;
- (void) raiseEvent: (void *)msgString, ...;
@end

@interface Warning_c : EventType_c
{
@public
  char *messageString;
}
/*** methods implemented in .m file ***/
- (void) setMessageString: (char *)str;
- (char *) getMessageString;
- (void) raiseEvent;
- (void) raiseEvent: (void *)msgString, ...;
@end

@interface Error_c : Warning_c // <Error>
/*** methods implemented in .m file ***/
- (void) raiseEvent;
- (void) raiseEvent: (void *)msgString, ...;
@end
