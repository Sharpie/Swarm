// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objc/objc.h>
#import <collections/StringObject.h>

BOOL
stringp (id obj)
{
  // conformsTo @protocol(String) doesn't work.
  return [obj isKindOfClassNamed: "String_c"];
}

BOOL
literal_string_p (id obj)
{
  return stringp (obj) && [obj getLiteralFlag];
}

BOOL
listp (id obj)
{
  // conformsTo @protocol(List) core dumps. isKindOf doesn't work either!
  return [obj isKindOfClassNamed: "List_linked"];
}

