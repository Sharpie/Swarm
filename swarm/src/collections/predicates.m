// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objc/objc.h>
#import <collections/StringObject.h>

BOOL
stringp (id obj)
{
  return [obj conformsTo: @protocol(String)];
}

BOOL
literal_string_p (id obj)
{
  return stringp (obj) && [obj getLiteralFlag];
}

BOOL
symbolp (id obj)
{
  return [obj conformsTo: @protocol(Symbol)];
}

BOOL
listp (id obj)
{
  return [obj conformsTo: @protocol(ArchiverList)];
}

BOOL
keywordp (id obj)
{
  return [obj conformsTo: @protocol(ArchiverKeyword)];
}

BOOL
valuep (id obj)
{
  return [obj conformsTo: @protocol(ArchiverValue)];
}

BOOL
arrayp (id obj)
{
  return [obj conformsTo: @protocol(ArchiverArray)];
}

BOOL
pairp (id obj)
{
  return [obj conformsTo: @protocol(ArchiverPair)];
}

BOOL
cons_literal_p (id obj)
{
  if (stringp (obj))
    {
      const char *str = [obj getC];
      
      return strcmp (str, "cons") == 0;
    }
  return NO;
}

BOOL
list_literal_p (id obj)
{
  if (stringp (obj))
    {
      const char *str = [obj getC];
      
      return strcmp (str, "list") == 0;
    }
  return NO;
}
