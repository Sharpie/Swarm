// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#import <simtools/MessageProbeWidget.h>
#import <simtools.h>
#import <defobj.h> // nameToObject
#import <gui.h>

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Avoid using chars as an index to ctype table.
#define isSpace(ch) isspace((int)ch)

@implementation MessageProbeWidget

+ createBegin: aZone
{
  id obj;
  
  obj = [super createBegin: aZone];
  [obj setMaxReturnWidth: 0];
  
  return obj;
}

- setObject: obj
{
  myObject = obj;
  return self;
}

- setProbe: (Probe *)theProbe
{
  myProbe = (MessageProbe *)theProbe;
  return self;
}

- setMaxReturnWidth: (int)width
{
  maxReturnWidth = width;
  return self;
}

- createEnd
{
  int i, which_arg;

#ifndef USE_FRAME
  widgetName = [parent makeWidgetNameFor: self];
  GUI_MAKE_FRAME (self);
#endif
  [super createEnd];

  if (![myProbe getHideResult])
    {
      result = [MessageProbeEntry createBegin: [self getZone]];
      [result setParent: self];
      [result setResultIdFlag: (BOOL)[myProbe isResultId]];
      result = [result createEnd];
      if (maxReturnWidth)
        [result setWidth: maxReturnWidth];
      [result setActiveFlag: NO];
    }
  
  argNum = [myProbe getArgNum];
  
  if (argNum)
    {
      objWindows = (int *)malloc (sizeof (int) * argNum);
      argNum *= 2; 
      myWidgets = (id <Widget> *)malloc (sizeof (id <Widget>) * argNum);
    }
  else
    myWidgets = (id <Widget> *)malloc (sizeof (id <Widget>));
  
  myWidgets[0] = [Button createParent: self];
  [(id <Button>)myWidgets[0] setButtonTarget: self
                method: @selector (dynamic)];
  [(id <Button>)myWidgets[0] setText: [myProbe getArgName: 0]];  
  [myWidgets[0] packFillLeft: argNum ? NO : YES];
  
  for (i = 1; i < argNum; i++)
    {
      which_arg = i / 2;
      
      if (i % 2)
        {
          objWindows[which_arg] = [myProbe isArgumentId: which_arg];
          myWidgets[i] = [MessageProbeEntry createBegin: [self getZone]];
          [myWidgets[i] setParent: self];
          [myWidgets[i] setResultIdFlag: [myProbe isArgumentId: which_arg]];
          [myWidgets[i] setArg: which_arg];
          myWidgets[i] = [myWidgets[i] createEnd];
        } 
      else
        {
          myWidgets[i] = [Label createParent: self];
          [(id <Label>)myWidgets[i] setText: [myProbe getArgName: which_arg]];
          [myWidgets[i] packFillLeft: NO];
        }
    }
  return self;
}

static BOOL
empty (const char *str)
{
  int i, length;
  
  if (str == NULL)
    return YES;
  
  length = strlen (str);
  for (i = 0; i < length; i++)
    if (!isSpace (str[i]))
      break;
  
  return (i >= length);
}

- dynamic
{
  int i;
  const char *result_string;
  
  for (i = 0; i < (argNum / 2); i++)
    {
      id <MessageProbeEntry> entryWidget = myWidgets[2 * i + 1];
      const char *test = strdup ([entryWidget getValue]);
      
      if (empty (test))
        {
          GUI_BEEP ();
          return self;
        }
      
      if (!objWindows[i])
        [myProbe setArg: i To: test];
    }

#if 0  
    // Here I must insist on a TCLOBJC mediated call since there will often
    // be situations where the probe might attempt a direct call thus casting
    // the result to an int (when the probedMessage does not take arguments).
    [myProbe _trueDynamicCallOn_: myObject resultStorage: &result_string];
#endif
    // probedSelector will be nil in the case of methods with arguments,
    // and _trueDynamicCallOn_ will be called instead.  MessageProbes
    // now support Id and Class, so the above isn't an issue. -mgd
    [myProbe dynamicCallOn: myObject resultStorage: &result_string];

  if (![myProbe getHideResult])
    {
      [result setActiveFlag: YES];
      if ([myProbe isResultId])
        {
          if ((resultObject = nameToObject (result_string)) != nil)
            [result setValue: [resultObject getIdName]];
          else    
            [result setValue: result_string];
        }
      else
        [result setValue: result_string];
      
      [result setActiveFlag: NO];
    }
  
  free ((void *)result_string);  
  [probeDisplayManager update];
  return self;
}

- Spawn
{
  if (resultObject != nil)
    CREATE_PROBE_DISPLAY (resultObject);
  else
    GUI_BEEP ();
  
  return self;
}

- argSpawn: (int) which
{
  id arg_obj;
  const char *id_name = [myProbe getArg: which];
  
  if (id_name != NULL)
    {
      arg_obj = nameToObject (id_name);
      CREATE_PROBE_DISPLAY (arg_obj);
    }
  else
    GUI_BEEP ();
  
  return self;
}

- update
{
  return self;
}

- (void)drop
{
  int i;

  for (i = 0; i < argNum; i++)
    [myWidgets[i] drop];
  
  [super drop];
}

- (const char *)package
{
  if (resultObject == nil)
    {
      GUI_BEEP ();
      return "";
    }
  return [resultObject getObjectName];
}

- (const char *)package: (int)which
{
  const char *id_name = [myProbe getArg: which];
  
  if (id_name == NULL) 
    {
      GUI_BEEP ();
      return "";
    }
  return id_name;
}

- (const char *)getId
{
  if (![myProbe getHideResult])
    return [result getValue];
  else
    return NULL;
}

- (const char *)getId: (int)which
{
  return [((id <MessageProbeEntry>)myWidgets [which * 2 + 1]) getValue];
}

- idReceive: (int)which
{
  id resObj = GUI_DRAG_AND_DROP_OBJECT ();

  [myProbe setArg: which ToObjectName: resObj];
  
  which *= 2;
  which += 1;

  [myWidgets[which] setActiveFlag: YES];
  [((id <Entry>)myWidgets[which]) setValue: [resObj getIdName]];
  [myWidgets[which] setActiveFlag: NO];

  GUI_UPDATE ();

  return self;
}

#ifndef USE_FRAME
- setParent: theParent
{
  parent = theParent;
  return self;
}

- (const char *)getWidgetName
{
  return widgetName;
}

- pack
{
  GUI_PACK (self);
  return self;
}
#endif

@end
