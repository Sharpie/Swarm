// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc/global.h>
#import <TkInterp.h>
#import <tkobjc/CheckButton.h>

inline int
stringIsFalse (const char *s)
{
  return s[0] == '0' && s[1] == '\0';
}

@implementation CheckButton

- createEnd
{
  [super createEnd];
  
  // create the checkbutton
  [globalTkInterp eval: "checkbutton %s;", widgetName];
  [globalTkInterp eval: "%s configure -variable %s;",
                  widgetName,
		  variableName];
  return self;
}

- setBoolValue: (BOOL)v
{
  if (v)
    [globalTkInterp eval: "%s select;", widgetName];
  else
    [globalTkInterp eval: "%s deselect;", widgetName];
  return self;
}

- setValue: (const char *)v
{
  return [self setBoolValue: stringIsFalse (v)];
}


// just ignore this entirely - does it mean anything?
- setWidth: (unsigned)w Height: (unsigned)h
{
  return self;
}

- (BOOL)getBoolValue
{
  return !stringIsFalse ([self getValue]);
}

// could do setvalue with Tcl select/deselect
@end
