// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
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
/*** methods in Symbol_c (inserted from .m file) ***/
- createEnd;
+ create: aZone setName: (char *)symbolName;
- (char *) getName;
- (void) describe: outputCharStream;
@end

@interface EventType_c : Symbol_c
/*** methods in EventType_c (inserted from .m file) ***/
- (void) raiseEvent;
- (void) raiseEvent: (void *)msgString, ...;
@end

@interface Warning_c : EventType_c
{
@public
  char *messageString;
}
/*** methods in Warning_c (inserted from .m file) ***/
- (void) setMessageString: (char *)messageStr;
- (char *) getMessageString;
- (void) raiseEvent;
- (void) raiseEvent: (void *)eventData, ...;
- (void) describe: outputCharStream;
@end

@interface Error_c : Warning_c
/*** methods in Error_c (inserted from .m file) ***/
- (void) raiseEvent;
- (void) raiseEvent: (void *)eventData, ...;
@end
