// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Symbol.m
Description:  classes to define distinct global id constants   
Library:      defobj
*/

#import <defobj/Symbol.h>
#import <collections.h>

#define __USE_FIXED_PROTOTYPES__ // for gcc headers
#include <stdio.h>
#include <string.h>
#include <stdarg.h>


@implementation Symbol_c

PHASE(Creating)

- createEnd
{
  raiseEvent( SourceMessage,
    "must use create:setName: to create a new symbol constant\n" );
  return nil;
}

+ create: aZone setName: (char *)symbolName
{
  Symbol_c *newSymbol;

  newSymbol = [aZone allocIVars: getNextPhase( self )];
  newSymbol->name = [aZone alloc: strlen( symbolName ) + 1];
  strcpy( newSymbol->name, symbolName );
  return newSymbol;
}

PHASE(Using)

- (char *) getName
{
  return name;
}

- (void) describe: outputCharStream
{
  char  buffer[100];

  [super describe: outputCharStream];
  sprintf( buffer, "> symbol name: %s\n", name );
  [outputCharStream catC: buffer];
}

@end


@implementation EventType_c

- (void) raiseEvent
{
  // currently ignored
}

- (void) raiseEvent: (void *)msgString, ...
{
  // currently ignored
}

@end


//
// printMessage() -- function shared by raiseEvent: in Warning_c and Error_c
//
static void printMessage( char *eventClass, char *eventName,
                          char *eventData, va_list argptr, char *messageString)
{
  char  *function, *file;
  int   line;

  fprintf( _obj_xerror, "*** event raised for %s: %s\n",
           eventClass, eventName );

  if ( eventData[0] == '\r' ) {
    function = eventData + 1;
    file = va_arg( argptr, char * );
    line = va_arg( argptr, int );
    fprintf( _obj_xerror,
             "*** function: %s(), file: %s, line: %d\n",
             function, file, line );
    eventData = va_arg( argptr, char * );
  }
  if ( eventData ) {
    vfprintf( _obj_xerror, eventData, argptr );
  } else if ( messageString ) {
    vfprintf( _obj_xerror, messageString, argptr );
  }
}

@implementation Warning_c

- (void) setMessageString: (char *)messageStr
{
  messageString = messageStr;
}

- (char *) getMessageString
{
  return messageString;
}

- (void) raiseEvent
{
  fprintf( _obj_xerror, "*** event raised for warning: %s\n", name );
  if ( messageString ) fputs( messageString, _obj_xerror );
  fprintf( _obj_xerror, "*** execution continuing...\n" );
}

- (void) raiseEvent: (void *)eventData, ...
{
  va_list argptr;

  if ( ! eventData ) [self raiseEvent];
  va_start( argptr, eventData );
  printMessage( "warning", name, eventData, argptr, messageString );
  fprintf( _obj_xerror, "*** execution continuing...\n" );
}

- (void) describe: outputCharStream
{
  char  buffer[100];

  [super describe: outputCharStream];
  if ( messageString ) {
    sprintf( buffer, "> default message:\n%s", messageString );
    [outputCharStream catC: buffer];
  } else {
    [outputCharStream catC: "> (no default message)\n"];
  }
}

@end


@implementation Error_c

- (void) raiseEvent
{
  fprintf( _obj_xerror, "*** event raised for error: %s\n", name );
  if ( messageString ) fputs( messageString, _obj_xerror );
  fprintf( _obj_xerror, "*** execution terminating due to error\n" );
  abort();
}

- (void) raiseEvent: (void *)eventData, ...
{
  va_list  argptr;

  if ( ! eventData ) [self raiseEvent];
  va_start( argptr, eventData );
  printMessage( "error", name, eventData, argptr, messageString );
  fprintf( _obj_xerror, "*** execution terminating due to error\n" );
  abort();
}

@end
