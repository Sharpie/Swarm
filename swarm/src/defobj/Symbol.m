// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

/*
Name:         Symbol.m
Description:  classes to define distinct global id constants   
Library:      defobj
*/

#import <defobj/Symbol.h>
#import <collections.h>

#include <misc.h> // strcpy, fprintf, va_start

// standard program-wide allocation zones

@implementation Symbol_c

PHASE(Creating)

- createEnd
{
  raiseEvent (SourceMessage,
    "> must use create:setName: to create a new symbol constant\n");
  return nil;
}

+ create: aZone setName: (const char *)symbolName
{
  Symbol_c *newSymbol;
  char *newNameBuf;

  newSymbol = [aZone allocIVars: getNextPhase (self)];
  newNameBuf = [aZone alloc: strlen (symbolName) + 1];
  strcpy (newNameBuf, symbolName);
  newSymbol->name = newNameBuf;
  return newSymbol;
}

PHASE(Using)

- (const char *)getName
{
  return SSTRDUP (name);
}

- (void) describe: outputCharStream
{
  [super describe: outputCharStream];
  [outputCharStream catC: ">symbol name: "];
  [outputCharStream catC: name];
  [outputCharStream catC: "\n"];
}

@end


@implementation EventType_c

- (void) raiseEvent
{
  // currently ignored
}

- (void) raiseEvent: (const void *)msgString, ...
{
  // currently ignored
}

@end


//
// printMessage() -- function shared by raiseEvent: in Warning_c and Error_c
//
static void
printMessage (const char *eventClass,
              const char *eventName,
              const void *eventData,
              va_list argptr,
              const char *messageString)
{
  const char *function, *file;
  int line;

  fprintf (_obj_xerror, "*** event raised for %s: %s\n",
           eventClass, eventName);

  if (((char *) eventData)[0] == '\r')
    {
      function = va_arg (argptr, char *);
      file = va_arg (argptr, char *);
      line = va_arg (argptr, int);
      fprintf (_obj_xerror,
               "*** function: %s(), file: %s, line: %d\n",
               function, file, line);
      eventData = va_arg (argptr, char *);
    }
  if (eventData) 
    vfprintf (_obj_xerror, eventData, argptr);
  else if (messageString)
    vfprintf (_obj_xerror, messageString, argptr);
}

@implementation Warning_c

- (void)setMessageString: (const char *)messageStr
{
  messageString = messageStr;
}

- (const char *)getMessageString
{
  return messageString;
}

- (void)raiseEvent
{
  fprintf (_obj_xerror, "*** event raised for warning: %s\n", name);
  if (messageString)
    fputs (messageString, _obj_xerror);
  fprintf (_obj_xerror, "*** execution continuing...\n");
}

- (void)raiseEvent: (const void *)eventData, ...
{
  va_list argptr;

  if (! eventData)
    [self raiseEvent];
  va_start (argptr, eventData);
  printMessage ("warning", name, eventData, argptr, messageString);
  fprintf (_obj_xerror, "*** execution continuing...\n");
}

- (void) describe: outputCharStream
{
  [super describe: outputCharStream];
  if (messageString) 
    {
      [outputCharStream catC: "> default message:\n"];
      [outputCharStream catC: messageString];
    }
  else
    [outputCharStream catC: "> (no default message)\n"];
}

@end


@implementation Error_c

- (void)raiseEvent
{
  fprintf (_obj_xerror, "*** event raised for error: %s\n", name);
  if (messageString)
    fputs (messageString, _obj_xerror);
  fprintf (_obj_xerror, "*** execution terminating due to error\n");
  abort();
}

- (void)raiseEvent: (const void *)eventData, ...
{
  va_list argptr;

  if (! eventData)
    [self raiseEvent];
  va_start (argptr, eventData);
  printMessage ("error", name, eventData, argptr, messageString);
  fprintf (_obj_xerror, "*** execution terminating due to error\n");
  abort();
}

@end
