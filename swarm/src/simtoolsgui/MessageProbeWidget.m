// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui/MessageProbeWidget.h>
#import <simtoolsgui.h>
#import <defobj.h> // nameToObject
#import <gui.h>
#import <objectbase.h> // val_t
#import <objc/objc-api.h>

#include <misc.h> // strlen, strdup, isspace

// Avoid using chars as an index to ctype table.
#define isSpace(ch) isspace((int)ch)

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
  
  switch (val.type)
    {
    case _C_ID: 
      return [val.val.object getIdName];
    case _C_SEL:
      return sel_get_name (val.val.selector);
    case _C_CHR:
      // character return is broken in libffi-1.18
      sprintf (buf, "%c", (char)val.val.sint);
      break;
    case _C_UCHR:
      // character return is broken in libffi-1.18
      switch (val.val.uint)
        {
        case NO:       
          sprintf (buf, "%o (NO)", (unsigned) val.val.uint);
          break;
        case YES:       
          sprintf (buf, "%o (YES)", (unsigned) val.val.uint);
          break;  
        default:
          sprintf (buf, "%o", (unsigned) val.val.uint);
          break;
        }
      break;
    case _C_SHT:
      // short return is broken in libffi-1.18
      sprintf (buf, "%hd", (short)val.val.sint);
      break;
    case _C_USHT:
      // short return is broken in libffi-1.18
      sprintf (buf, "%hu", (unsigned short)val.val.uint);
      break;
    case _C_INT:
      sprintf (buf, "%d", val.val.sint);
      break;
    case _C_UINT:
      sprintf (buf, "%u", val.val.sint);
      break;
    case _C_LNG:
      sprintf (buf, "%ld", val.val.slong);
      break;
    case _C_ULNG:
      sprintf (buf, "%lu", val.val.ulong);
      break;
    case _C_FLT:
      sprintf (buf, "%f", val.val._float);
      break;
    case _C_DBL:
      sprintf (buf, "%f", val.val._double);
      break;
    case _C_VOID:
      strcpy (buf, "none");
      break;
    default:
      abort ();
    }
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

#ifndef USE_FRAME
  widgetName = [parent makeWidgetNameFor: self];
  GUI_MAKE_FRAME (self);
#endif
  [super createEnd];

  if (![myProbe getHideResult])
    {
      resultMessageProbeEntry = [MessageProbeEntry createBegin: [self getZone]];
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
      objWindows = (BOOL *)xmalloc (sizeof (BOOL) * argCount);
      argCount *= 2; 
      myWidgets = (id <Widget> *)xmalloc (sizeof (id <Widget>) * argCount);
    }
  else
    myWidgets = (id <Widget> *)xmalloc (sizeof (id <Widget>));
  
  myWidgets[0] = [Button createParent: self];
  [(id <Button>)myWidgets[0] setButtonTarget: self
                method: @selector (dynamic)];
  [(id <Button>)myWidgets[0] setText: [myProbe getArgName: 0]];  
  [myWidgets[0] packFillLeft: argCount ? NO : YES];
  
  for (i = 1; i < argCount; i++)
    {
      which_arg = i / 2;
      
      if (i % 2)
        {
          objWindows[which_arg] = [myProbe isArgumentId: which_arg];
          myWidgets[i] = [MessageProbeEntry createBegin: [self getZone]];
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

- dynamic
{
  int i;
  
  for (i = 0; i < (argCount / 2); i++)
    {
      id <MessageProbeEntry> entryWidget = myWidgets[2 * i + 1];
      const char *test = strdup ([entryWidget getValue]);
      
      if (empty (test))
        {
          GUI_BEEP ();
          return self;
        }
      
      if (!objWindows[i])
        [myProbe setArg: i ToString: test];
    }

  {
    val_t ret = [myProbe dynamicCallOn: myObject];
  
    if (![myProbe getHideResult])
      {
        [resultMessageProbeEntry setActiveFlag: YES];
        [resultMessageProbeEntry setValue: printVal (ret)];
        if (ret.type == _C_ID)
          resultObject = ret.val.object;
        [resultMessageProbeEntry setActiveFlag: NO];
      }
  }
  [probeDisplayManager update];
  return self;
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
 
  if (val.type == _C_ID)
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
  
  if (val.type != _C_ID)
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
  return [((id <MessageProbeEntry>)myWidgets [which * 2 + 1]) getValue];
}

- idReceive: (const char *)windowName arg: (int)which
{
  id resObj = GUI_DRAG_AND_DROP_OBJECT ();

  [myProbe setArg: which ToString: [resObj getObjectName]];
  
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
