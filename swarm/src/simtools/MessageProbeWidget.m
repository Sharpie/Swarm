// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#import <string.h>
#import <ctype.h>
#import <stdio.h>
#import <malloc.h>
#import <stdlib.h> // for alpha
#import <simtools/MessageProbeWidget.h>
#import <simtools/global.h>
#import <tkobjc/control.h>
#import <simtools.h>

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
  myProbe = (MessageProbe *) theProbe;
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
  char bcmd[1024];
  
  [super createEnd];
  
  if (![myProbe getHideResult])
    {
      result = [Entry createParent: self];
      tkobjc_disabledState (result);
      if (maxReturnWidth)
        tkobjc_setWidth (result, maxReturnWidth);
      if ([myProbe isResultId])
        {
          tkobjc_bindButton3ToSpawn (result, self, 1);
          dragAndDrop (result, self);
        }
      else
        tkobjc_bindButton3ToBeUnhelpful (result, self);
      tkobjc_packFillLeft (result, 0);
    }
  
  argNum = [myProbe getArgNum];
  
  if (argNum)
    {
      objWindows = (int *) malloc (sizeof (int)*argNum);
      argNum *= 2; 
      myWidgets = (Widget **) malloc (sizeof (Widget *)*argNum);
    }
  else
    myWidgets = (Widget **) malloc (sizeof (Widget *));
  
  myWidgets[0] = [Button createParent: self];
  
  bcmd[0] = '\0';
  strcat (bcmd, tclObjc_objectToName (self));
  strcat (bcmd, " dynamic");
  [(Button *)myWidgets[0] setCommand: bcmd];

  tkobjc_setText (myWidgets[0], [myProbe getArgName: 0]);
  
  if (argNum)
    tkobjc_packFillLeft (myWidgets[0], 0);
  else 
    tkobjc_packFillLeft (myWidgets[0], 1);
  
  for (i = 1; i < argNum; i++)
    {
      which_arg = i / 2;
      
      if (i % 2)
        {
          myWidgets[i] = [Entry createParent: self];
          if ([myProbe isArgumentId: which_arg])
            {
              objWindows[which_arg] = 1;
              tkobjc_disabledState (myWidgets[i]);
              tkobjc_bindButton3ToArgSpawn (myWidgets[i], self, which_arg);
              dragAndDropArg (myWidgets[i], self, which_arg);
            }
          else
            {
              objWindows[which_arg] = 0;
              tkobjc_bindButton3ToBeUnhelpful (myWidgets[i], nil);
            }
          tkobjc_packFillLeft (myWidgets[i], 1);
        } 
      else
        {
          myWidgets[i] = [Label createParent: self];
          tkobjc_setText (myWidgets[i], [myProbe getArgName: which_arg]);
          tkobjc_packFillLeft (myWidgets[i], 0);
        }
    }
  return self;
}

int
empty (const char *str)
{
  int i, length;
  
  if (str == NULL)
    return 1;
  
  length = strlen (str);
  for (i = 0; i < length; i++)
    if (!isSpace (str[i]))
      break;
  
  return (i >= length);
}

- dynamic
{
  int i;
  const char *test, *result_string;
  
  for (i = 0; i < (argNum / 2); i++)
    {
      test = strdup ([((Entry *) myWidgets[2*i + 1]) getValue]);
      
      if (empty (test))
        {
          tkobjc_ringBell ();
          return self;
        }
      
      if (!objWindows[i])
        [myProbe setArg: i To: test];
    }
  
  // Here I must insist on a TCLOBJC mediated call since there will often
  // be situations where the probe might attempt a direct call thus casting
  // the result to an int (when the probedMessage does not take arguments).
  [myProbe _trueDynamicCallOn_: myObject resultStorage: &result_string];
  
  if (![myProbe getHideResult])
    {
      tkobjc_normalState (result);      
      if ([myProbe isResultId])
        {
          if ((resultObject = tclObjc_nameToObject (result_string)) != nil)
            {
              if ([resultObject respondsTo: @selector (getInstanceName)])
                [result setValue: [resultObject getInstanceName]];
              else
                [result setValue: [resultObject name]];
            }
          else    
            [result setValue: result_string];
        }
      else
        [result setValue: result_string];
      
      tkobjc_disabledState (result);
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
    tkobjc_ringBell ();
  
  return self;
}

- argSpawn: (int) which
{
  id arg_obj;
  const char *id_name;
  
  id_name = [myProbe getArg: which];
  
  if (id_name != NULL)
    {
      arg_obj = tclObjc_nameToObject (id_name);
      CREATE_PROBE_DISPLAY (arg_obj);
    }
  else
    tkobjc_ringBell ();
  
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
      tkobjc_ringBell ();
      return "";
    }
  return tclObjc_objectToName (resultObject);
}

- (const char *)package: (int) which
{
  const char *id_name;
  
  id_name = [myProbe getArg: which];
  
  if (id_name == NULL) 
    {
      tkobjc_ringBell ();
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

- (const char *)getId: (int) which
{
  return [((Entry *) myWidgets [which*2 + 1]) getValue];
}

- idReceive: (int) which
{
  id resObj = tkobjc_gimme_drag_and_drop_object ();

  [myProbe setArg: which To: strdup (tclObjc_objectToName (resObj))];
  
  which *= 2;
  which += 1;

  tkobjc_normalState (myWidgets[which]);

  if ([resObj respondsTo: @selector (getInstanceName)])
    [((Entry *)myWidgets[which]) setValue: [resObj getInstanceName]];
  else
    [((Entry *)myWidgets[which]) setValue: [resObj name]];
  
  tkobjc_disabledState (myWidgets[which]);
  tkobjc_update ();
  return self;
}

@end
