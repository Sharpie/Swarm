// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <stdio.h> // sprintf, sscanf

#import <objc/objc.h>
#import <objc/objc-api.h>
#import <objectbase/VarProbe.h>
#import <defobj.h> // Warning
#import "local.h"

#include <misc.h> // strdup, strcmp, xmalloc, XFREE

@implementation VarProbe

PHASE(Creating)

- setProbedVariable: (const char *)aVariable
{
  if (probedVariable)
    {
      if (SAFEPROBES)
        {
          [Warning raiseEvent: "It is an error to reset the variable\n"];
          return nil;
        }
      else 
        XFREE (probedVariable);     // memory allocation?
    }
  probedVariable = strdup (aVariable);	   // make a local copy
  return self;
}

- createEnd
{
  IvarList_t ivarList;
  int i;
  
  [super createEnd];

  if (SAFEPROBES)
    if (probedVariable == 0 || probedClass == 0)
      {
        [Warning raiseEvent: 
                   "VarProbe object was not properly initialized\n"];
        return nil;
      }
  
  ivarList = probedClass->ivars;
  
  // search the ivar list for the requested variable.
  i = 0;
  while (i < ivarList->ivar_count
         && strcmp (ivarList->ivar_list[i].ivar_name, probedVariable) != 0)
    i++;
  
  if (i == ivarList->ivar_count)
    { 
      // if not found
      if (SAFEPROBES)
        [Warning raiseEvent: "Warning: variable not found\n"];
      return nil;
    }
  else
    {
      probedType = ivarList->ivar_list[i].ivar_type;
      dataOffset = ivarList->ivar_list[i].ivar_offset;
      
      if (probedType[0] == _C_CHARPTR
          || probedType[0] == _C_CHR
          || probedType[0] == _C_UCHR
          || probedType[0] == _C_INT
          || probedType[0] == _C_UINT
          || probedType[0] == _C_LNG
          || probedType[0] == _C_ULNG
          || probedType[0] == _C_FLT
          || probedType[0] == _C_DBL)
        interactiveFlag = YES;
      else
        interactiveFlag = NO;
      
      // set up default formatting string for floating point and 
      // double types - defaults are set in the probeLibrary instance
      if  (probedType[0] == _C_FLT || probedType[0] == _C_DBL)
        {
          char *buf = xmalloc (7);
          sprintf (buf, "%%.%dg", [probeLibrary getDisplayPrecision]); 
          floatFormat = buf; // allocate memory for string
        }
      return self;
    }
}

PHASE(Setting)

- setNonInteractive
{
  interactiveFlag = NO;

  return self;
}

- setStringReturnType: returnType
{
  stringReturnType = returnType;
  return self;
}

- setFloatFormat: (const char *)format
{
  if (probedType[0] == _C_FLT || probedType[0] == _C_DBL) 
    floatFormat = strdup (format);
  else
    [Warning raiseEvent: "%s is not a float or double\n", probedVariable];

  return self;
}

PHASE(Using)

- (const char *)getProbedVariable
{
  return probedVariable;
}

- (BOOL)getInteractiveFlag
{
  return interactiveFlag;
}

- (int)getDataOffset
{
  return dataOffset;
}

- free
{
  if (probedVariable)
    XFREE (probedVariable);
  return [super free];
}

- clone: aZone
{
  VarProbe *new_probe;
  
  new_probe = [VarProbe createBegin: aZone];
  [new_probe setProbedClass: probedClass];
  [new_probe setProbedVariable: probedVariable];
  if (objectToNotify != nil) 
    [new_probe setObjectToNotify: objectToNotify];
  new_probe = [new_probe createEnd];
  
  [new_probe setStringReturnType: stringReturnType];
  [new_probe setFloatFormat: floatFormat];
  
  return new_probe;
}

// no guarantees about alignment here.
- (void *)probeRaw: anObject
{
  if (safety)
    if (![anObject isKindOf: probedClass])
      [Warning raiseEvent: "VarProbe for class %s tried on class %s\n",
               [probedClass name], [anObject name]];
  return (char *)anObject + dataOffset;
}

- (void *)probeAsPointer: anObject
{
  void *p;
  void *q = NULL;
  
  if (safety)
    if (![anObject isKindOf: probedClass])
      [Warning raiseEvent: "VarProbe for class %s tried on class %s\n",
               [probedClass name],
               [anObject name]];
  
  p = ((char *)anObject) + dataOffset;
  
  switch (probedType[0])
    {
    case _C_ID:      q = (void *) *(id *)p; break;
    case _C_CLASS:   q = (void *) *(Class *)p; break;
    case _C_CHARPTR:
    case _C_PTR:     q = (void *) *(void **)p; break;
    case _C_INT: 
    case _C_UINT:
    case _C_LNG:
    case _C_ULNG:
      q = (void *) *(long *)p; break;
    default:
      if (SAFEPROBES)
        [Warning raiseEvent: "Invalid type %s to retrieve as a pointer...\n",
                 probedType];
      break;
    }
  return q;
}

- (int)probeAsInt: anObject
{
  const void *p;
  int i = 0;
  
  if (safety)
    if (![anObject isKindOf: probedClass])
      [Warning raiseEvent: "VarProbe for class %s tried on class %s\n",
               [probedClass name], [anObject name]];
  
  p = ((const char *)anObject) + dataOffset;
  
  switch (probedType[0])
    {
    case _C_ID:   i = (long)*(id *)p; break;
    case _C_CHARPTR:
    case _C_PTR:  i = (long)*(void **)p; break;

    case _C_UCHR: i = (int)*(unsigned char *)p; break;
    case _C_CHR:  i = (int)*(char *)p; break;
      
    case _C_INT:  i = (int)*(int *)p; break;
    case _C_UINT: i = (unsigned)*(unsigned int *)p; break;

    case _C_LNG:  i = (long)*(long *)p; break;
    case _C_ULNG: i = (unsigned long)*(unsigned long *)p; break;
      
    default:
      if (SAFEPROBES)
        [Warning raiseEvent: "Invalid type %s to retrieve as an int...\n",
                 probedType];
      break;
    }
  return i;
}

- (double)probeAsDouble: anObject
{
  const void *p;
  double d = 0.0;
  
  if (safety)
    if (![anObject isKindOf: probedClass])
      [Warning raiseEvent: "VarProbe for class %s tried on class %s\n",
               [probedClass name],
               [anObject name]];
  
  p = ((const char *)anObject) + dataOffset;
  
  switch (probedType[0])
    {
    case _C_UCHR: d = (double)*(unsigned char *)p; break;
    case _C_CHR:  d = (double)*(char *)p; break;
      
    case _C_INT:  d = (double)*(int *)p; break;
    case _C_UINT: d = (double)*(unsigned int *)p; break;

    case _C_LNG:  d = (double)*(long *)p; break;
    case _C_ULNG: d = (double)*(unsigned long *)p; break;
      
    case _C_FLT:  d = (double)*(float *)p; break;
    case _C_DBL:  d = (double)*(double *)p; break;

    default:
      if (SAFEPROBES)
        [Warning raiseEvent: "Invalid type %s to retrieve as a double...\n",
                 probedType];
      break;
    }
  return d;
}

- (const char *)probeAsString: anObject Buffer: (char *)buf
{
  // by default - use precision set by -setFormatFloat 
  // as number of digits to use in formatting the string
  [self probeAsString: anObject Buffer: buf withFullPrecision: 0];
  return buf;
}

- (const char *) probeAsString: anObject
                        Buffer: (char *)buf 
             withFullPrecision: (int)precision
{
  const void *p;
  
  if (safety)
    if (![anObject isKindOf: probedClass])
      sprintf (buf, "VarProbe for class %s tried on class %s\n",
               [probedClass name], [anObject name]);
  
  p = (const char *)anObject + dataOffset; // probeData
  
  switch (probedType[0])
    {
    case _C_ID:
      if (!(*(id *)p))
        sprintf (buf, "nil");
      else 
        if ([*(id *)p respondsTo: @selector (getInstanceName)])
          sprintf (buf, "%s", [*(id *)p getInstanceName]);
        else
          sprintf (buf, "%s", [*(id *)p name]);
      break;
    case _C_CLASS:
      if (!(*(Class *)p))
        sprintf (buf, "nil");
      else
        sprintf (buf, "%s", (*(Class *)p)->name );
      break;
    case _C_PTR:
      sprintf (buf, "0x%p", *(void **)p);
      break;
    case _C_UCHR:
      if (stringReturnType == DefaultString)
        sprintf (buf, "%u '%c'",(unsigned)*(unsigned char *)p,
                *(unsigned char *)p);
      else if (stringReturnType == CharString)
        sprintf (buf, "'%c'",*(unsigned char *)p);
      else if (stringReturnType == IntString)
        sprintf (buf, "%u",(unsigned)*(unsigned char *)p);
      else
        {
          printf ("stringReturnType set incorrectly!!!\n");
          exit (-1);
        }
      break;
    case _C_CHR:
      if (stringReturnType == DefaultString)
        sprintf (buf, "%d '%c'",(int)*(char *)p, *(char *)p);
      else if (stringReturnType == CharString)
        sprintf (buf, "'%c'",*(char *)p);
      else if (stringReturnType == IntString)
        sprintf (buf, "%d",(int)*(char *)p);
      else
        {
          printf ("stringReturnType set incorrectly!!!\n");
          exit (-1);
        }
      break;
    case _C_INT:
      sprintf (buf, "%d", *(int *)p);
      break;
    case _C_UINT:
      sprintf (buf, "%u", *(unsigned *)p);
      break;
    case _C_LNG:
      sprintf (buf, "%ld", *(long *)p);
      break;
    case _C_ULNG:
      sprintf (buf, "%lu", *(unsigned long *)p);
      break;
    case _C_FLT:
      if (precision)
        sprintf (buf, "%.*g", [probeLibrary getSavedPrecision],
                 (double)(*(float *)p));
      else
        sprintf (buf, floatFormat, (double)(*(float *)p));
      break;
    case _C_DBL:
      if (precision)
        sprintf (buf, "%.*g", [probeLibrary getSavedPrecision],
                *(double *)p);
      else
        sprintf (buf, floatFormat, *(double *)p);
      break;
    case _C_CHARPTR:
      sprintf (buf, "%s", *(char **)p ? *(char **)p : "<NULL>");
      break;
    default:
      sprintf (buf, "..."); 
      break;
    }
  return buf;
}

// sets the probed to whatever is pointed to by newValue. Use the
// type information to try to do this intelligently.
- setData: anObject To: (void *)newValue
{
  const void *p;

  if (safety)
    if (![anObject isKindOf: probedClass])
      [Warning raiseEvent: "VarProbe for class %s tried on class %s\n",
               [probedClass name], [anObject name]];
  
  p = (const char *)anObject + dataOffset;		  // probeData
  
  switch (probedType[0])
    {
    case _C_ID:   *(id *)p = *(id *)newValue; break;
    case _C_CHARPTR:
    case _C_PTR:  *(void **)p = *(void **)newValue; break;
      
    case _C_UCHR: *(unsigned char *)p = *(unsigned char *)newValue; break;
    case _C_CHR:  *(char *)p = *(char *)newValue; break;
      
    case _C_INT:  *(int *)p = *(int *)newValue; break;
    case _C_UINT: *(unsigned int *)p = *(unsigned int *)newValue; break;
    case _C_LNG:  *(long *)p = *(long *)newValue; break;
    case _C_ULNG: *(unsigned long *)p = *(unsigned long *)newValue; break;
    case _C_FLT:  *(float *)p = *(float *)newValue; break;
    case _C_DBL:  *(double *)p = *(double *)newValue; break;
      
    default:
      if (SAFEPROBES)
        [Warning raiseEvent: "Invalid type %s to set\n", probedType];
      break;
    }
  
  if (objectToNotify != nil)
    {
      if ([objectToNotify respondsTo: M(forEach:)])
        {
          id index, tempObj;
          index = [objectToNotify begin: scratchZone];
          while ((tempObj = [index next]) != nil)
            {
              [tempObj eventOccurredOn: anObject
                       via: self
                       withProbeType: "VarProbe"
                       on: probedVariable
                       ofType: probedType[0]
                       withData: newValue];
            }
          [index drop];
        }
      else 
        [objectToNotify eventOccurredOn: anObject
                        via: self
                        withProbeType: "VarProbe"
                        on: probedVariable
                        ofType: probedType[0]
                        withData: newValue];
    }
  return self;
}

// sets data to the string passed in. Some duplicated code with
// setData:To:, but it's not too bad. Note we don't allow setting
// pointers here, because textual representations of pointers are
// strange. That's probably not a good idea.
- (BOOL)setData: anObject ToString: (const char *)s
{
  union {
    char c;
    int i;
    unsigned int ui;
    float f;
    double d;
    long l;
    unsigned long ul;
  } value;
  int rc = 0;
  void *p;
  
  if (safety)
    if (![anObject isKindOf: probedClass])
      [Warning raiseEvent: "VarProbe for class %s tried on class %s\n",
               [probedClass name], [anObject name]];

  p = (char *)anObject + dataOffset;		  // probeData

  switch (probedType[0])
    {
    case _C_CHR:
      if (stringReturnType == CharString)
        {
          if ((rc = sscanf (s, "'%c'", &value.c)) == 1)
            *(char *)p = value.c;
	} 
      else 
        {
	  if ((rc = sscanf (s, "%d", &value.i)) == 1)
	    *(char *)p = value.i;
	}
      break;
      
    case _C_UCHR:
      if (stringReturnType == CharString)
        {
          if ((rc = sscanf (s, "'%c'", &value.c)) == 1)
            *(char *)p = value.c;
        }
      else 
        {
          if ((rc = sscanf (s, "%u", &value.i)) == 1)
            *(unsigned char *)p = value.i;
	}
      break;
      
  case _C_CHARPTR:
      *(char **)p = strdup (s) ;
      rc = (*(char **)p != NULL);
      break;
      
    case _C_INT:
      if ((rc = sscanf (s, "%d", &value.i)) == 1) 
        *(int *)p = value.i; 
      break;
      
    case _C_UINT:
      if ((rc = sscanf (s, "%u", &value.ui)) == 1) 
        *(unsigned int *)p = value.ui; 
      break;

    case _C_LNG:
      if ((rc = sscanf (s, "%ld", &value.l)) == 1) 
        *(long *)p = value.i; 
      break;
      
    case _C_ULNG:
      if ((rc = sscanf (s, "%lu", &value.ul)) == 1) 
        *(unsigned long *)p = value.ul; 
      break;
      
    case _C_FLT:
      if ((rc = sscanf (s, "%f", &value.f)) == 1) 
        *(float *)p = value.f; 
      break;

    case _C_DBL:
      if ((rc = sscanf (s, "%lf", &value.d)) == 1) 
        *(double *)p = value.d; 
      break;
      
    default:
      if (SAFEPROBES)
        [Warning raiseEvent: "Invalid type %s to set\n", probedType];
      break;
  }

  if (rc != 1 && SAFEPROBES)
    {
      [Warning raiseEvent: "Error scanning for value in string %s\n", s];
      return NO;
    }
  
  if (objectToNotify != nil)
    {
      if ([objectToNotify respondsTo: M(forEach:)])
        {
          id index, tempObj;

          index = [objectToNotify begin: scratchZone];
          while ((tempObj = [index next]) != nil)
            {
              [tempObj eventOccurredOn: anObject
                       via: self
                       withProbeType: "VarProbe"
                       on: probedVariable
                       ofType: probedType[0]
                       withData: (void *)s];
            }
          [index drop];
        }
      else 
        [objectToNotify eventOccurredOn: anObject
                        via: self
                        withProbeType: "VarProbe"
                        on: probedVariable
                        ofType: probedType[0]
                        withData: (void *)s];
    }
  return YES;
}

@end
