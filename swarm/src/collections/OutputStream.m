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

/*
Name:         OutputStream.m
Description:  character string object   
Library:      collections
*/

#import <collections/OutputStream.h>
#import <defobj/internal.h> // lisp_type_for_objc_type
#import <defobj/defalloc.h> // getZone
#import <collections.h> // ArchiverValue, ArchiverKeyword, ArchiverQuoted
#import <collections/StringObject.h>
#include <misc.h> // FILE, fputs, isPrint
#include <swarmconfig.h> // PTRHEXFMT

@implementation OutputStream_c

PHASE(Creating)

+ createBegin: aZone
{
  OutputStream_c  *newStream;

  newStream = [aZone allocIVars: self];
  return newStream;
}

- setFileStream: (FILE *)file
{
  fileStream = file;
  return self;
}

- setExprFlag: (BOOL)exprFlag
{
  if (exprFlag)
    {
      expr = nil;
      exprStack = [List create: getZone (self)];
    }
  return self;
}

- createEnd
{
  createByCopy ();
  setNextPhase (self);
  return self;
}

+ create: aZone setFileStream: (FILE *)file
{
  OutputStream_c  *newStream;

  newStream = [aZone allocIVars: getNextPhase (self)];
  newStream->fileStream = file;
  return newStream;
}

PHASE(Using)

- (FILE *)getFileStream
{
  return fileStream;
}

- getExpr
{
  return expr;
}

- (void)_addExpr_: theExpr
{
  if ([exprStack getCount] == 0)
    expr = theExpr;
  else
    [[exprStack getFirst] addLast: theExpr];
}

#define ADDEXPR(theExpr) [self _addExpr_: theExpr]
#define ADDVALUE(type,value) ADDEXPR([[[ArchiverValue createBegin: getZone (self)] set##type: value] createEnd])
#define ADDCASTINT(value) ADDEXPR([[[ArchiverValue createBegin: getZone (self)] setLongLong: (long long) value] createEnd])

- (void)catC: (const char *)string
{
  if (!exprStack)
    fputs (string, fileStream);
}

- (void)catLiteral: (const char *)string
{
  if (exprStack)
    ADDEXPR ([[String create: getZone (self) setC: string]
               setLiteralFlag: YES]);
  else
    [self catC: string];
}

- (void)catBoolean: (BOOL)aBool
{
  if (exprStack)
    ADDVALUE (Boolean, aBool);
  else
    [self catC: aBool ? "#t" : "#f"];
  
}

- (void)catChar: (char)ch
{
  if (exprStack)
    ADDVALUE (Char, ch);
  else
    {
      char buf[DSIZE(char) + 1];

      [self catC: "#\\"];
      {
        if (isPrint ((unsigned char) ch))
          {
            buf[0] = (unsigned char) ch;
            buf[1] = '\0';
          }
        else
          sprintf (buf, "%03o", (unsigned) ch);
      }
      [self catC: buf];
    }
}

- (void)catFloat: (float)flt
{
  if (exprStack)
    ADDVALUE (Float, flt);
  else
    {
      char buf[20 + 1];
      sprintf (buf, "%fF0", flt);
      [self catC: buf];
    }
}

- (void)catDouble: (double)dbl
{
  if (exprStack)
    ADDVALUE (Double, dbl);
  else
    {
      char buf[30 + 1];
      sprintf (buf, "%fD0", dbl);
      [self catC: buf];
    }
}

- (void)catLongDouble: (long double)ldbl
{
  if (exprStack)
    ADDVALUE (LongDouble, ldbl);
  else
    {
      char buf[30 + 1];
      sprintf (buf, "%fL0", (double) ldbl);
      [self catC: buf];
    }
}

- (void)catInt: (int)i
{
  if (exprStack)
    ADDCASTINT (i);
  else
    {
      char buf[DSIZE (int) + 1];
      sprintf (buf, "%d", i);
      [self catC: buf];
    }
}

- (void)catUnsigned: (unsigned)un
{
  if (exprStack)
    ADDCASTINT (un);
  else
    {
      char buf[DSIZE (unsigned) + 1];
      sprintf (buf, "%u", un);
      [self catC: buf];
    }
}

- (void)catShort: (short)sht
{
  if (exprStack)
    ADDCASTINT (sht);
  else
    {
      char buf[DSIZE (short) + 1];
      sprintf (buf, "%hd", sht);
      [self catC: buf];
    }
}

- (void)catUnsignedShort: (unsigned short)usht
{
  if (exprStack)
    ADDCASTINT (usht);
  else
    {
      char buf[DSIZE (unsigned short) + 1];
      sprintf (buf, "%hu", usht);
      [self catC: buf];
    }
}

- (void)catLong: (long)lng
{
  if (exprStack)
    ADDCASTINT (lng);
  else
    {
      char buf[DSIZE (long) + 1];
      sprintf (buf, "%ld", lng);
      [self catC: buf];
    }
}

- (void)catUnsignedLong: (unsigned long)ulng
{
  if (exprStack)
    ADDCASTINT (ulng);
  else
    {
      char buf[DSIZE (unsigned long) + 1];
      sprintf (buf, "%ld", ulng);
      [self catC: buf];
    }
}

- (void)catLongLong: (long long)lnglng
{
  if (exprStack)
    ADDVALUE (LongLong, lnglng);
  else
    {
      char buf[DSIZE (long long) + 1];
      
#if SIZEOF_LONG_LONG != SIZEOF_LONG
#ifdef LLFMT
      sprintf (buf, "%" LLFMT "d", lnglng);
#else
      raiseEvent (NotImplemented, "No printf format specifier for long long");
#endif
#else
      sprintf (buf, "%ld", (long) lnglng);
#endif
      [self catC: buf];
    }
}

- (void)catUnsignedLongLong: (unsigned long long)ulnglng
{
  if (exprStack)
    ADDCASTINT (ulnglng);
  else
    {
      char buf[DSIZE (unsigned long long) + 1];
      
#if SIZEOF_LONG_LONG != SIZEOF_LONG
#ifdef LLFMT
      sprintf (buf, "%" LLFMT "u", ulnglng);
#else
      raiseEvent (NotImplemented, "Not format specifier for unsigned long long");
#endif
#else
      sprintf (buf, "%lu", (long) ulnglng);
#endif
      [self catC: buf];
    }
}

- (void)catPointer: (void *)ptr
{
  if (exprStack)
    abort ();
  else
    {
      char buf[20];
      
      sprintf (buf, PTRHEXFMT, ptr);
      [self catC: buf];
    }
}

- (void)catStartExpr
{
  if (exprStack)
    [exprStack addFirst: [ArchiverList create: getZone (self)]];
  else
    [self catC: "("];
}

- (void)catEndExpr
{
  if (exprStack)
    ADDEXPR ([exprStack removeFirst]);
  else
    [self catC: ")"];
}

- (void)catSeparator
{
  if (!exprStack)
    [self catC: " "];
}

- (void)catKeyword: (const char *)keyword
{
  if (exprStack)
    ADDEXPR ([[[ArchiverKeyword createBegin: getZone (self)]
                setKeywordName: keyword]
               createEnd]);
  else
    {
      [self catC: "#:"];
      [self catC: keyword];
    }
}

- (void)catSymbol: (const char *)symbolName
{
  if (exprStack)
    ADDEXPR ([[[ArchiverQuoted createBegin: getZone (self)]
               setQuotedObject: [[String create: getZone (self) 
                                         setC: symbolName]
                                  setLiteralFlag: YES]]
               createEnd]);
  else
    {
      [self catC: "'"];
      [self catC: symbolName];
    }
}

- (void)catString: (const char *)str
{
  if (exprStack)
    ADDEXPR ([[String create: getZone (self) setC: str] setLiteralFlag: YES]);
  else
    {
      [self catC: "\""];
      [self catC: str];
      [self catC: "\""];
    }
}

- (void)catClass: (Class)class_
{
  if (exprStack)
    ADDVALUE (Class, class_);
  else
    {
      [self catC: "<"];
      [self catC: swarm_class_getName(class_)];
      [self catC: ">"];
    }
}

- (void)catArrayRank: (unsigned)rank
{
  if (!exprStack)
    {
      [self catC: "#"];
      [self catUnsigned: rank];
    }
}

- (void)catEndArray
{
  if (exprStack)
    ADDEXPR ([[(id <ArchiverArray>)[ArchiverArray createBegin: getZone (self)]
                setArray: [[exprStack getFirst] removeLast]]
               createEnd]);
}

- (void)catArrayType: (const char *)type
{
  [self catStartExpr];
  [self catLiteral: "array"];
  [self catSeparator];
  [self catSymbol: lisp_type_for_objc_type (type, NULL)];
  {
    void outputCount (unsigned dim, unsigned count)
      {
        [self catSeparator];
        [self catUnsigned: count];
      }
    lisp_type_for_objc_type (type, outputCount);
  }
  [self catEndExpr];
}

- (void)catType: (const char *)type
{
  if (*type == _C_ARY_B)
    [self catArrayType: type];
  else
    [self catSymbol: lisp_type_for_objc_type (type, NULL)];
}

- (void)catStartCons
{
  [self catStartFunction: "cons"];
}

- (void)catEndCons
{
  if (exprStack)
    {
      id consExpr = [exprStack removeFirst];

      [consExpr removeFirst];
      ADDEXPR ([[[[[ArchiverPair createBegin: getZone (self)]
                  setConsFormatFlag: YES]
                  setCar: [consExpr getFirst]]
                  setCdr: [consExpr getLast]]
                 createEnd]);
      [consExpr drop];
    }
  else
    [self catC: ")"];
}

- (void)catStartParse
{
  [self catStartFunction: "parse"];
}

- (void)catEndParse
{
  [self catEndFunction];
}

- (void)catStartList
{
  [self catStartFunction: "list"];
}

- (void)catEndList
{
  [self catEndFunction];
}

- (void)catStartQuotedList
{
  if (exprStack)
    [self catStartExpr];
  else
    [self catC: "'("];
}

- (void)catEndQuotedList
{
  if (exprStack)
    ADDEXPR ([[[ArchiverQuoted createBegin: getZone (self)]
                setQuotedObject: [exprStack removeFirst]]
               createEnd]);
  else
    [self catC: ")"];
}

- (void)catStartFunction: (const char *)functionName
{
  [self catStartExpr];
  [self catLiteral: functionName];
}

- (void)catEndFunction
{
  [self catEndExpr];
}

- (void)catStartMakeInstance: (const char *)typeName
{
  [self catStartFunction: MAKE_INSTANCE_FUNCTION_NAME];
  [self catSeparator];
  [self catSymbol: typeName];
}

- (void)catEndMakeInstance
{
  [self catEndFunction];
}

- (void)catStartMakeClass: (const char *)className
{
  [self catStartFunction: MAKE_CLASS_FUNCTION_NAME];
  [self catSeparator];
  [self catSymbol: className];
}

- (void)catEndMakeClass
{
  [self catEndFunction];
}

- (void)catUnsignedPair: (unsigned)x : (unsigned)y
{
  if (!exprStack)
    {
      [self catC: "'"];
      [self catStartExpr];
      [self catUnsigned: x];
      [self catSeparator];
      [self catC: "."];
      [self catSeparator];
      [self catUnsigned: y];
      [self catEndExpr];
    }
  else
    {
      id car = [[[ArchiverValue createBegin: getZone (self)]
                  setLongLong: (long long) x]
                 createEnd];
      id cdr = [[[ArchiverValue createBegin: getZone (self)]
                  setLongLong: (long long) y]
                 createEnd];
      ADDEXPR ([[[ArchiverQuoted createBegin: getZone (self)]
                  setQuotedObject: 
                    [[[[ArchiverPair createBegin: getZone (self)]
                   setCar: car]
                  setCdr: cdr]
                      createEnd]]
                 createEnd]);
    }
}

- (void)catNil
{
  if (exprStack)
    ADDEXPR ([[[ArchiverValue createBegin: getZone (self)]
                setNil]
               createEnd]);
  else
    [self catC: "nil"];
}
@end
