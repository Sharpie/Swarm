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
symbolp (id obj)
{
  return [obj isKindOfClassNamed: "Symbol_c"];
}

BOOL
listp (id obj)
{
  // conformsTo @protocol(List) core dumps. isKindOf doesn't work either!
  return [obj isKindOfClassNamed: "List_linked"];
}

BOOL
keywordp (id obj)
{
  return [obj isKindOfClassNamed: "ArchiverKeyword_c"];
}

BOOL
valuep (id obj)
{
  return [obj isKindOfClassNamed: "ArchiverValue_c"];
}

BOOL
arrayp (id obj)
{
  return [obj isKindOfClassNamed: "ArchiverArray_c"];
}

BOOL
pairp (id obj)
{
  return [obj isKindOfClassNamed: "ArchiverPair_c"];
}
