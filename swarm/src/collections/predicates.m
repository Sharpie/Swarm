// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <defobj/swarm-objc-api.h>
#import <collections/StringObject.h>

BOOL
stringp (id obj)
{
  return [obj conformsTo: @protocol (String)];
}

BOOL
literal_string_p (id obj)
{
  return stringp (obj) && [obj getLiteralFlag];
}

BOOL
symbolp (id obj)
{
  return [obj conformsTo: @protocol (Symbol)];
}

BOOL
archiver_list_p (id obj)
{
  return [obj conformsTo: @protocol (ArchiverList)];
}

BOOL
keywordp (id obj)
{
  return [obj conformsTo: @protocol (ArchiverKeyword)];
}

BOOL
valuep (id obj)
{
  return [obj conformsTo: @protocol (ArchiverValue)];
}

BOOL
nil_value_p (id obj)
{
  if (valuep (obj))
    return [obj getObject] == nil;
  else
    return NO;
}

BOOL
arrayp (id obj)
{
  return [obj conformsTo: @protocol (ArchiverArray)];
}

BOOL
pairp (id obj)
{
  return [obj conformsTo: @protocol (ArchiverPair)];
}

BOOL
quotedp (id obj)
{
  return [obj conformsTo: @protocol (ArchiverQuoted)];
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

BOOL
quote_literal_p (id obj)
{
  if (stringp (obj))
    {
      const char *str = [obj getC];
      
      return strcmp (str, "quote") == 0;
    }
  return NO;
}
