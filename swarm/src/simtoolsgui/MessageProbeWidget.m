// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui/MessageProbeWidget.h>
#import <simtoolsgui.h>
#import <defobj.h> // nameToObject, STRDUP
#import <gui.h>
#import <objectbase.h> // val_t
#import <objc/objc-api.h>

#include <misc.h> // strlen, isSpace
#include "../objectbase/probing.h" // string_convert

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

static const char *
printVal (val_t val)
{
  static char buf[128];
  
  string_convert (val.type,
                  &val.val,
                  NULL,
                  [probeLibrary getDisplayPrecision],
                  DefaultString, buf);
  return buf;
}

@implementation MessageProbeWidget

PHASE(Creating)

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

- setProbe: (id <Probe>)theProbe
{
  myProbe = (id <MessageProbe>) theProbe;

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
  id aZone = [self getZone];

#ifndef USE_FRAME
  widgetName = [parent makeWidgetNameFor: self];
  GUI_MAKE_FRAME (self);
#endif
  [super createEnd];

  if (![myProbe getHideResult])
    {
      resultMessageProbeEntry = [MessageProbeEntry createBegin: aZone];
      [resultMessageProbeEntry setParent: self];
      [resultMessageProbeEntry setIdFlag: [myProbe isResultId]];
      resultMessageProbeEntry = [resultMessageProbeEntry createEnd];
      if (maxReturnWidth)
        [resultMessageProbeEntry setWidth: maxReturnWidth];
      [resultMessageProbeEntry setActiveFlag: NO];
    }
  
  argCount = [myProbe getArgCount];
  
  if (argCount)
    {
      objWindows = (BOOL *) [aZone alloc: sizeof (BOOL) * argCount];
      argCount *= 2; 
      myWidgets = 
        (id <Widget> *) [aZone alloc: sizeof (id <Widget>) * argCount];
    }
  else
    myWidgets = (id <Widget> *) [aZone alloc: sizeof (id <Widget>)];
  
  myWidgets[0] = [Button createParent: self];
  [(id <Button>)myWidgets[0] setButtonTarget: self method: M(dynamic)];
  [(id <Button>)myWidgets[0] setText: [myProbe getArgName: 0]];  
  [myWidgets[0] packFillLeft: argCount ? NO : YES];
  
  for (i = 1; i < argCount; i++)
    {
      which_arg = i / 2;
      
      if (i % 2)
        {
          objWindows[which_arg] = [myProbe isArgumentId: which_arg];
          myWidgets[i] = [MessageProbeEntry createBegin: aZone];
          [myWidgets[i] setParent: self];
          [myWidgets[i] setIdFlag: [myProbe isArgumentId: which_arg]];
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

PHASE(Using)

- (void)dynamic
{
  int i;
  
  for (i = 0; i < (argCount / 2); i++)
    {
      id <MessageProbeEntry> entryWidget = myWidgets[2 * i + 1];
      const char *test = STRDUP ([entryWidget getValue]);
      
      if (empty (test))
        {
          GUI_BEEP ();
          return;
        }
      
      if (!objWindows[i])
        [myProbe setArg: i ToString: test];
    }

  {
    val_t ret = [myProbe dynamicCallOn: myObject];
  
    if (ret.type != fcall_type_void && ![myProbe getHideResult])
      {
        [resultMessageProbeEntry setActiveFlag: YES];
        [resultMessageProbeEntry setValue: printVal (ret)];
        if (ret.type == fcall_type_object)
          resultObject = ret.val.object;
        [resultMessageProbeEntry setActiveFlag: NO];
      }
  }
  [probeDisplayManager update];
}

- Spawn: (const char *)widgetName
{
  if (resultObject != nil)
    CREATE_PROBE_DISPLAY (resultObject);
  else
    GUI_BEEP ();
  
  return self;
}

- argSpawn: (const char *)widgetName arg: (int)which
{
  val_t val = [myProbe getArg: which];
 
  if (val.type == fcall_type_object)
    CREATE_PROBE_DISPLAY (val.val.object);
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

  for (i = 0; i < argCount; i++)
    [myWidgets[i] drop];
  
  [super drop];
}

- (const char *)package: (const char *)windowName
{
  if (resultObject == nil)
    {
      GUI_BEEP ();
      return "";
    }
  return [resultObject getObjectName];
}

- (const char *)package: (const char *)windowName arg: (int)which
{
  val_t val = [myProbe getArg: which];
  
  if (val.type != fcall_type_object)
    {
      GUI_BEEP ();
      return "";
    }
  return [val.val.object getObjectName];
}

- (const char *)getId: (const char *)windowName
{
  if (![myProbe getHideResult])
    return [resultMessageProbeEntry getValue];
  else
    return NULL;
}

- (const char *)getId: (const char *)windowName arg: (int)which
{
  return [((id <MessageProbeEntry>) myWidgets [which * 2 + 1]) getValue];
}

- idReceive: (const char *)windowName arg: (int)which
{
  id resObj = GUI_DRAG_AND_DROP_OBJECT ();

  [myProbe setArg: which ToString: [resObj getObjectName]];
  
  which *= 2;
  which += 1;

  [myWidgets[which] setActiveFlag: YES];
  [((id <Entry>) myWidgets[which]) setValue: [resObj getDisplayName]];
  [myWidgets[which] setActiveFlag: NO];

  GUI_UPDATE ();

  return self;
}

#ifndef USE_FRAME
- setParent: (id <Frame>)theParent
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
