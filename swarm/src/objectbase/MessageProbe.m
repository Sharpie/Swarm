// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#import <stdio.h>
#import <stdlib.h>
#import <string.h>

#import <tclObjc.h>
#import <tkobjc/control.h> // tkobjc_dynamicEval

#import "MessageProbe.h"
#import "swarm_rts_routines.h"

// SAFEPROBES enables lots of error checking here.
#define SAFEPROBES 1

const char *
copy_to_nth_colon (const char *str, int n)
{
  int count = -1;
  int beginning,end,i;
  char *new_str;

  for (i = 0; i < n; i++)
    while (str[++count] != ':');
  
  count++;
  
  beginning = count;

  while (str[count] != ':')
    count++;
  
  count++;
  
  end = count;

  new_str = malloc ((end - beginning) + 1);

  count = 0;
  for (i = beginning; i < end; i++)
    new_str[count++] = str[i];
  new_str[count] = '\0';

  return new_str;
}


@implementation MessageProbe

+ createBegin: aZone
{
  MessageProbe *obj;

  obj = [super createBegin: aZone];
  [obj setHideResult: 0];
  obj->objectToNotify = nil;
  return obj;
}

- setProbedSelector: (SEL) aSel
{
  if (probedSelector)
    {
      if (SAFEPROBES)
        {
          fprintf(stderr, "It is an error to reset the selector\n");
          return nil;
        }
    }
  probedSelector = aSel;
  return self;
}

- setProbedMessage: (const char *) aMessage
{
  raiseEvent (WarningMessage,
             "A bug in gcc-2.7.2 makes it unadvisable to make a message\n"
             "probe with a string represetation. For now, use\n"
              "setProbedSelector instead.");
  
  if (probedMessage)
    {
      if (SAFEPROBES)
        {
          fprintf(stderr, "It is an error to reset the message\n");
          return nil;
        } 
      else 
        free ((void *)probedMessage);  // memory allocation?
    }
  probedMessage = strdup (aMessage);   // make a local copy
  return self;
}

- (const char *)getProbedMessage
{
  return probedMessage;
}

- setHideResult: (int) val
{
  hr = val;
  return self;
}

- (int) getHideResult
{
  return hr;
}

static char runtimeBugWarning[] = "Could not complete creation of a message probe because of a bug in gcc 2.7.2.\nContinuing anyway: see documentation for details.\n";

- createEnd
{
  int i;
  
  [super createEnd];

  caching = 0;
  intImp = 0;
  doubleImp = 0;
  
  if (probedSelector)
    probedMessage = strdup (sel_get_name (probedSelector));
  
  if (SAFEPROBES)
    if (probedMessage == 0)
      {
        fprintf(stderr,"MessageProbe object was not properly initialized\n");
        return nil;
      }
  
  // Here I get the typing information... So I need to overwrite my selector
  // with one that has typing information attached.
  probedSelector = sel_get_any_typed_uid (probedMessage);
  
  if (!probedSelector)
    {
      raiseEvent (WarningMessage, runtimeBugWarning);
      [self drop]; 
      return nil; // I've just done a Roger Switcheroo!!! Yoo Hoo!
    }
  
  if (!sel_get_type(probedSelector))
    {
      raiseEvent (WarningMessage, runtimeBugWarning);
      [self drop]; 
      return nil; // I've just done a Roger Switcheroo!!! Yoo Hoo!
    }
  
  probedType = strdup (sel_get_type(probedSelector));
  
  argNum = get_number_of_arguments(probedType) - 2;
  
  if(!argNum)
    {
      switch (probedType[0])
        {
        case _C_FLT: returnCategory = 1; break;
        case _C_DBL: returnCategory = 2; break;                        
          
        case _C_ID:
        case _C_CLASS:
        case _C_PTR:
        case _C_UCHR:
        case _C_CHR:
        case _C_INT:
        case _C_UINT:
        case _C_CHARPTR:
        default: returnCategory = 0; break; 
        }
      argLabels = (const char **)malloc (sizeof (const char *));
      argLabels[0] = probedMessage;
    }
  else
    {
      probedSelector = 0;
      argLabels = (const char **)malloc (argNum * sizeof (const char *));
      arguments = (const char **)malloc (argNum * sizeof (const char *));
      
      // Since I depend on arguments[i] being NULL in the situations where
      // it was never set, I must ensure that this is the case be initialising
      // them all to NULL (the language definition is supposed to take care
      // of this but I have been burnt by this kind of thing in the past...),
      for (i = 0; i < argNum; i++)
        {
          arguments[i] = NULL;
          argLabels[i] = copy_to_nth_colon(probedMessage,i);
        }
    }
  return self;
}

- free
{
  if (probedMessage)
    free ((void *)probedMessage);
  return [super free];
}

-clone: aZone
{
  MessageProbe * new_probe;
  
  new_probe = [MessageProbe createBegin: aZone];
  [new_probe setProbedClass: probedClass];
  [new_probe setProbedSelector: probedSelector];
  [new_probe setHideResult: hr];
  if (objectToNotify != nil) 
    [new_probe setObjectToNotify: objectToNotify];
  new_probe = [new_probe createEnd];

  return new_probe;
}

-(int) getArgNum
{
  return argNum;
}

- (const char *)getArg: (int) which
{
  if (SAFEPROBES)
    if (which >= argNum)
      {
        fprintf (stderr,
                 "Attempted to get argument #%d, but %s has only %d arguments!\n",
                 which, probedMessage, argNum);
        return NULL;
      }
  
  return arguments[which];
}


- setArg: (int)which To: (const char *)what
{
  if (SAFEPROBES)
    if (which >= argNum)
      {
        fprintf (stderr,
                 "Attempted to set argument #%d, but %s has only %d arguments!!!\n",
                 which,probedMessage,argNum);
        return self;
      }
  
  arguments[which] = what;
  return self;
}

- (const char *)getArgName: (int)which
{
  
  if (SAFEPROBES)
    if ((which >= argNum) && which)
      {
        fprintf(stderr,
                "Attempted to get name of argument #%d, but %s has only %d arguments!!!\n",
                which, probedMessage, argNum);
        return NULL;
      }
  
  return argLabels[which];
}

- (int)isResultId
{
  return (probedType[0] == _C_ID);
}

- (int)isArgumentId: (int) which
{
  const char *the_type;
  int i;
  
  if (SAFEPROBES)
    if (which >= argNum) 
      {
        fprintf(stderr,
                "Attempted to verify whether argument #%d is of type ID, but %s has only %d arguments!!!\n",
                which, probedMessage, argNum);
        return 0;
      }
  
  the_type = probedType;
  the_type =  my_objc_skip_argspec(the_type);  // result 
  the_type =  my_objc_skip_argspec(the_type);  // object
  the_type =  my_objc_skip_argspec(the_type);  // selector  

  for (i = 0; i < which; i++)
    the_type =  my_objc_skip_argspec(the_type);
  
  return (the_type[0] == _C_ID);
}

- _setImp_: anObject
{
  switch (returnCategory)
    {
    case 0 : 
      intImp = (IntImp) [anObject methodFor: probedSelector];  
      floatImp = 0;
      doubleImp = 0;
      break;
    case 1 :
      floatImp = (FloatImp) [anObject methodFor: probedSelector];  
      intImp = 0;
      doubleImp = 0;
      break;
    case 2 :
      doubleImp = (DoubleImp) [anObject methodFor: probedSelector];
      intImp = 0;
      floatImp = 0;
      break;
    default :
      fprintf (stderr,"Major problem with returnCategory in MessageProbe!\n");
      exit (-1);
      break;
    }
  
  return self;
}

- updateMethodCache: anObject
{
  if (anObject == nil)
    {
      caching = 0; 
      return self;
    }
  
  [self _setImp_: anObject];
  
  return self;
}

- _trueDynamicCallOn_: target resultStorage: (const char **)result
{
  int i;
  
  char cmd[1024];
  
  cmd[0] = '\0';
  
  strcat (cmd, tclObjc_objectToName (target));
  strcat (cmd," ");

  if (!argNum)
    strcat (cmd,argLabels[0]);
  else
    for (i = 0; i < argNum; i++)
      {
        strcat (cmd,argLabels[i]);
        strcat (cmd," ");
        strcat (cmd,arguments[i]);
        strcat (cmd," ");
      }
  
  *result = tkobjc_dynamicEval (cmd);
  
  // since other routines call this so-called internal 
  // method _trueDynamicCallOn_, I have to put this here.  If and when
  // the probing is brought in line with our coding standards, this
  // hook section should be moved.
  if (objectToNotify != nil)
    {
      if ([objectToNotify respondsTo: M(forEach:)])
        {
          id index, tempObj;

          index = [objectToNotify begin: scratchZone];
          while ((tempObj = [index next]) != nil)
            {
              [tempObj eventOccurredOn: target
                       via: self
                       withProbeType: "MessageProbe"
                       on: probedMessage
                       ofType: probedType[0]
                       withData: (void *)cmd];
            }
          [index drop];
        }
      else 
        [objectToNotify eventOccurredOn: target
                        via: self
                        withProbeType: "MessageProbe"
                        on: probedMessage
                        ofType: probedType[0]
                        withData: cmd];
    }
  return self;
}

- dynamicCallOn: target
{
  const char *tmp;
  
  if (probedSelector)
    {
      if (!caching)
        [self _setImp_: target];
      
      switch (returnCategory)
        {
        case 0: 
          (*intImp) (target, probedSelector);
          break;
        case 1:
          (*floatImp) (target, probedSelector);
          break;
        case 2:
          (*doubleImp) (target, probedSelector);
          break;
        default:
          fprintf (stderr,"Major problem with returnCategory in MessageProbe!\n");
          exit(-1);
          break;
        }
    }
  else
    {
      [self _trueDynamicCallOn_: target resultStorage: &tmp];
      free ((void *)tmp); 
    }
  return self;
}

- dynamicCallOn: target resultStorage: (const char **)result
{
  if (probedSelector)
    {
       // how else do I decide the amount of space necessary???
      char *buf = malloc (100);
      
      if (!caching)
        [self _setImp_: target];

      switch (returnCategory)
        {
        case 0: 
          sprintf (buf, "%d", (*intImp) (target, probedSelector));
          break;
        case 1:
          sprintf (buf, "%f", (*floatImp) (target, probedSelector));
          break;
        case 2:
          sprintf (buf, "%f", (*doubleImp) (target, probedSelector));
          break;
        default:
          fprintf (stderr, "Major problem with returnCategory in MessageProbe!\n");
          exit (-1);
          break;
        }
      *result = buf;
    }
  else
    [self _trueDynamicCallOn_: target resultStorage: result];
  
  return self;
}

- (int)intDynamicCallOn: target
{
  const char *tmp;
  int val;

  if (probedSelector)
    {
      if (!caching)
        [self _setImp_: target];
      
      switch(returnCategory)
        {
        case 0: 
          return (*intImp)(target, probedSelector);
          break;
        case 1:
          return ((int) ((*floatImp)(target, probedSelector)));
          break;
        case 2:
          return ((int) ((*doubleImp)(target, probedSelector)));
          break;
        default:
          fprintf (stderr, "Major problem with returnCategory in MessageProbe!\n");
          exit(-1);
          break;
        }
    }
  else 
    {
      [self _trueDynamicCallOn_: target resultStorage: &tmp];
      sscanf (tmp, "%d", &val);
      free ((void *)tmp);
      return val;
    }
}

- (float)floatDynamicCallOn: target
{
  const char *tmp;
  float val;

  if (probedSelector)
    {
      if (!caching)
        [self _setImp_: target];
      
      switch (returnCategory)
        {
        case 0: 
          return ((float) (*intImp)(target, probedSelector));
          break;
        case 1:
          return (*floatImp)(target, probedSelector);
          break;
        case 2:
          return ((float) ((*doubleImp)(target, probedSelector)));
          break;
        default:
          fprintf (stderr,"Major problem with returnCategory in MessageProbe!\n");
          exit(-1);
          break;
        }
    }
  else
    {
      [self _trueDynamicCallOn_: target resultStorage: &tmp];
      sscanf (tmp, "%f", &val);
      free ((void *)tmp);
      return val;
    }
}

- (double) doubleDynamicCallOn: target
{
  const char *tmp;
  double val;
  
  if (probedSelector)
    {
      if (!caching)
        [self _setImp_: target];
      
      switch (returnCategory)
        {
        case 0: 
          return ((double) (*intImp)(target, probedSelector));
          break;
        case 1:
          return ((double)(*floatImp)(target, probedSelector));
          break;
        case 2:
          return (*doubleImp)(target, probedSelector);
          break;
        default:
          fprintf (stderr,"Major problem with returnCategory in MessageProbe!\n");
          exit(-1);
          break;
        }
    }
  else
    {
      [self _trueDynamicCallOn_: target resultStorage: &tmp];
      sscanf (tmp, "%lf", &val);
      free ((void *)tmp);
      return val;
    }
}

@end
