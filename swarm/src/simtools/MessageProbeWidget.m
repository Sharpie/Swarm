// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
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
#import <simtools/ProbeDisplay.h>
#import <simtools/global.h>
#import <tkobjc/control.h>

@implementation MessageProbeWidget

+ createBegin: aZone
{
  id obj ;
  
  obj = [super createBegin: aZone];
  [obj setMaxReturnWidth: 0];
  
  return obj ;
}

- setObject: obj
{
  myObject = obj ;
  return self ;
}

- setProbe: (Probe *)theProbe
{
  myProbe = (MessageProbe *) theProbe;
  return self;
}

- setMaxReturnWidth: (int)width
{
  maxReturnWidth = width ;
  return self ;
}

- createEnd
{
  int i, which_arg ;
  char bcmd[1024] ;
  
  [super createEnd] ;
  
  if (![myProbe getHideResult])
    {
      result = [Entry createParent: self];
      disabledState (result);
      if (maxReturnWidth)
        setWidth (result, maxReturnWidth);
      if ([myProbe isResultId])
        {
          [globalTkInterp
            eval:
              "bind %s <Button-3> {focus %s ; %s configure -highlightcolor red ;"
            "update ; %s Spawn ; %s configure -highlightcolor black ;"
            "update ; focus %s ; update } ;",
            [result getWidgetName],
            [result getWidgetName],
            [result getWidgetName], 
            tclObjc_objectToName(self),
            [result getWidgetName],
            [self getWidgetName]];
          dragAndDrop (result, self);
        }
      else
        {
          [globalTkInterp
            eval:
              "bind %s <Button-3> {focus %s; %s configure -highlightcolor red ;"
            "update ; bell ; update ; %s configure -highlightcolor black ;"
            "update ; focus %s ; update} ;",
            [result getWidgetName],
            [result getWidgetName],
            [result getWidgetName], 
            [result getWidgetName],
            [self getWidgetName]] ;
        }
      packFillLeft (result, 0);
    }
  
  argNum = [myProbe getArgNum] ;
  
  if (argNum)
    {
      objWindows = (int *) malloc(sizeof(int)*argNum);
      argNum *= 2; 
      myWidgets = (Widget **) malloc(sizeof(Widget *)*argNum);
    }
  else
    myWidgets = (Widget **) malloc(sizeof(Widget *));
  
  myWidgets[0] = [Button createParent: self];
  
  bcmd[0] = '\0' ;
  strcat (bcmd,tclObjc_objectToName(self));
  strcat (bcmd," dynamic") ;
  [(Button *)myWidgets[0] setCommand: bcmd];

  setText (myWidgets[0], [myProbe getArgName: 0]);
  
  if(argNum)
    packFillLeft (myWidgets[0], 0);
  else 
    packFillLeft (myWidgets[0], 1);
  
  for (i = 1; i < argNum; i++)
    {
      which_arg = i / 2 ;
      
      if (i % 2)
        {
          myWidgets[i] = [Entry createParent: self] ;
          if ([myProbe isArgumentId: which_arg])
            {
              objWindows[which_arg] = 1 ;
              disabledState (myWidgets[i]);
              
              [globalTkInterp
                eval:
                  "bind %s <Button-3> {focus %s ; %s configure -highlightcolor red ;"
                "update ; %s argSpawn: %d ; %s configure -highlightcolor black ;"
                "update ; focus %s ; update } ;",
                [myWidgets[i] getWidgetName],
                [myWidgets[i] getWidgetName],
                [myWidgets[i] getWidgetName], 
                tclObjc_objectToName(self),
                which_arg,
                [myWidgets[i] getWidgetName],
                [self getWidgetName]];
              
              dragAndDropArg (myWidgets[i], self, which_arg);
            }
          else
            {
              objWindows[which_arg] = 0;
              [globalTkInterp
                eval:
                  "bind %s <Button-3> {focus %s; %s configure -highlightcolor red ;"
                "update ;"
                "bell ; update ; "
                "%s configure -highlightcolor black ;"
                "update} ;",
                [myWidgets[i] getWidgetName],
                [myWidgets[i] getWidgetName],
                [myWidgets[i] getWidgetName], 
                [myWidgets[i] getWidgetName]] ;
              
              [globalTkInterp eval: 
                                "bind %s <FocusIn> {%s selection range 0 end} ;"
                              "bind %s <FocusOut> {%s selection clear}",
                              [myWidgets[i] getWidgetName],
                              [myWidgets[i] getWidgetName],
                              [myWidgets[i] getWidgetName],
                              [myWidgets[i] getWidgetName]];
            }
          packFillLeft (myWidgets[i], 1);
        } 
      else
        {
          myWidgets[i] = [Label createParent: self] ;
          setText (myWidgets[i], [myProbe getArgName: which_arg]);
          packFillLeft (myWidgets[i], 0);
        }
    }
  return self;
}

int
empty (const char *str)
{
  int i, length  ;
  
  if (str == NULL)
    return 1 ;
  
  length = strlen(str);
  for (i = 0 ; i < length ; i++)
    if(!isspace(str[i]))
      break;
  
  return (i >= length);
}

- dynamic
{
  int i;
  char *test, *result_string;
  
  for (i = 0; i < (argNum / 2); i++)
    {
      test = strdup ([((Entry *) myWidgets[2*i + 1]) getValue]);
      
      if (empty (test))
        {
          ringBell ();
          return self;
        }
      
      if (!objWindows[i])
        [myProbe setArg: i To: test];
    }
  
  // Here I must insist on a TCLOBJC mediated call since there will often
  // be situations where the probe might attempt a direct call thus casting
  // the result to an int (when the probedMessage does not take arguments).
  [myProbe _trueDynamicCallOn_: myObject resultStorage: &result_string] ;
  
  if (![myProbe getHideResult])
    {
      normalState (result);      
      if ([myProbe isResultId])
        {
          if ((resultObject = tclObjc_nameToObject(result_string)) != nil)
            {
              if ([resultObject respondsTo: @selector(getInstanceName)])
                [result setValue: (char *)[resultObject getInstanceName]];
              else
                [result setValue: (char *)[resultObject name]];
            }
          else    
            [result setValue: result_string];
        }
      else
        [result setValue: result_string];
      
      disabledState (result);
    }
  
  free (result_string);  
  [probeDisplayManager update];
  return self;
}

- Spawn
{
  if (resultObject != nil)
    [probeDisplayManager createProbeDisplayFor: resultObject];
  else
    ringBell ();
  
  return self;
}

- argSpawn: (int) which
{
  id arg_obj ;
  char *id_name ;
  
  id_name = [myProbe getArg: which] ;
  
  if (id_name != NULL)
    {
      arg_obj = tclObjc_nameToObject (id_name);
      [probeDisplayManager createProbeDisplayFor: arg_obj];
    }
  else
    ringBell ();
  
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
  
  [super drop] ;
}

- (const char *)package
{
  if (resultObject == nil)
    {
      ringBell ();
      return "" ;
    }
  return tclObjc_objectToName(resultObject);
}

- (const char *)package: (int) which
{
  const char *id_name;
  
  id_name = [myProbe getArg: which];
  
  if (id_name == NULL) 
    {
      ringBell ();
      return "";
    }
  return id_name;
}

- (const char *)getId
{
  if (![myProbe getHideResult])
    return [result getValue] ;
  else
    return NULL ;
}

- (const char *)getId: (int) which
{
  return [((Entry *) myWidgets[which*2 + 1]) getValue] ;
}

- idReceive: (int) which
{
  id resObj;
  char *objName;

  objName = strdup ([[globalTkInterp eval: "gimme $DDOBJ"] result]) ;
  resObj = tclObjc_nameToObject (objName) ;
  
  [myProbe setArg: which To: objName] ;
  
  which *= 2 ;
  which += 1 ;

  normalState (myWidgets[which]);
  if ([resObj respondsTo: @selector(getInstanceName)])
    [((Entry *)myWidgets[which]) 
      setValue: (char *)[resObj getInstanceName]];
  else
    [((Entry *)myWidgets[which]) 
       setValue: (char *)[resObj name]];
  
  disabledState (myWidgets[which]);
  update ();
  return self;
}

@end
