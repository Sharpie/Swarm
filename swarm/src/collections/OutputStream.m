// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
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
#include <misc.h> // FILE, fputs
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

- (void)catChar: (char)ch
{
  char buf[DSIZE(char) + 1];
  [self catC: "#\\"];
  {
    if (isprint ((unsigned char)ch))
      {
        buf[0] = (unsigned char)ch;
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
  raiseEvent (NotImplemented, "Not format specifier for long long");
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

@end
