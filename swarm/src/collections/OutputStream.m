// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         OutputStream.m
Description:  character string object   
Library:      collections
*/

#import <collections/OutputStream.h>
#import <collections/InputStream.h>
#import <collections/predicates.h>
#import <defobj/internal.h> // lisp_type_for_objc_type
#include <misc.h> // FILE, fputs, isPrint
#include <swarmconfig.h> // PRINTF_LL_FMT, PTRUINT, PTRUINTFMT

@implementation OutputStream_c

PHASE(Creating)

+ createBegin: aZone
{
  OutputStream_c  *newStream;

  newStream = [aZone allocIVars: self];
  return newStream;
}

- (void) setFileStream: (FILE *)file
{
  fileStream = file;
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

- (void)catC: (const char *)cstring
{
  fputs (cstring, fileStream);
}

- (void)catBoolean: (BOOL)bool
{
  [self catC: bool ? "#t" : "#f"];
}

- (void)catChar: (char)ch
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

- (void)catFloat: (float)flt
{
  char buf[20 + 1];
  sprintf (buf, "%fF0", flt);
  [self catC: buf];
}

- (void)catDouble: (double)dbl
{
  char buf[30 + 1];
  sprintf (buf, "%fD0", dbl);
  [self catC: buf];
}

- (void)catLongDouble: (long double)ldbl
{
  char buf[30 + 1];
  sprintf (buf, "%fL0", (double) ldbl);
  [self catC: buf];
}

- (void)catInt: (int)i
{
  char buf[DSIZE (int) + 1];
  sprintf (buf, "%d", i);
  [self catC: buf];
}

- (void)catUnsigned: (unsigned)un
{
  char buf[DSIZE (unsigned) + 1];
  sprintf (buf, "%u", un);
  [self catC: buf];
}

- (void)catShort: (short)sht
{
  char buf[DSIZE (short) + 1];
  sprintf (buf, "%hd", sht);
  [self catC: buf];
}

- (void)catUnsignedShort: (unsigned short)usht
{
  char buf[DSIZE (unsigned short) + 1];
  sprintf (buf, "%hu", usht);
  [self catC: buf];
}

- (void)catLong: (long)lng
{
  char buf[DSIZE (long) + 1];
  sprintf (buf, "%ld", lng);
  [self catC: buf];
}

- (void)catUnsignedLong: (unsigned long)ulng
{
  char buf[DSIZE (unsigned long) + 1];
  sprintf (buf, "%ld", ulng);
  [self catC: buf];
}

- (void)catLongLong: (long long)lnglng
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

- (void)catUnsignedLongLong: (unsigned long long)ulnglng
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

- (void)catPointer: (void *)ptr
{
  char buf[20];

  sprintf (buf, PTRHEXFMT, ptr);
  [self catC: buf];
}

- (void)catStartExpr
{
  [self catC: "("];
}

- (void)catEndExpr
{
  [self catC: ")"];
}

- (void)catSeparator
{
  [self catC: " "];
}

- (void)catKeyword: (const char *)keyword
{
  [self catC: "#:"];
  [self catC: keyword];
}

- (void)catSymbol: (const char *)symbol
{
  [self catC: "'"];
  [self catC: symbol];
}

- (void)catString: (const char *)str
{
  [self catC: "\""];
  [self catC: str];
  [self catC: "\""];
}

- (void)catClass: (const char *)className
{
  [self catC: "<"];
  [self catC: className];
  [self catC: ">"];
}

- (void)catArrayRank: (unsigned)rank
{
  [self catC: "#"];
  [self catUnsigned: rank];
}

- (void)catType: (const char *)type
{
  if (*type == _C_ARY_B)
    {
      [self catC: "(array '"];
      [self catC: lisp_type_for_objc_type (type, NULL)];
      {
        void outputCount (unsigned dim, unsigned count)
          {
            [self catSeparator];
            [self catUnsigned: count];
          }
        lisp_type_for_objc_type (type, outputCount);
      }
      [self catC: ")"];
    }
  else
    [self catSymbol: lisp_type_for_objc_type (type, NULL)];
}

- (void)catStartCons
{
  [self catStartExpr];
  [self catC: "cons"];
}

- (void)catStartParse
{
  [self catStartExpr];
  [self catC: "parse"];
}

- (void)catStartList
{
  [self catStartExpr];
  [self catC: "list"];
}

- (void)catStartQuotedList
{
  [self catC: "'("];
}

- (void)catStartMakeInstance: (const char *)typeName
{
  [self catStartExpr];
  [self catC: MAKE_INSTANCE_FUNCTION_NAME];
  [self catSeparator];
  [self catSymbol: typeName];
}

- (void)catStartMakeClass: (const char *)className
{
  [self catStartExpr];
  [self catC: MAKE_CLASS_FUNCTION_NAME];
  [self catSeparator];
  [self catSymbol: className];
}

- (void)catUnsignedPair: (unsigned)x : (unsigned)y
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
@end

