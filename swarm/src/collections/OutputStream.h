// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:        OutputStream.h
Description: character string object
Library:     collections
*/

#import <defobj/Create.h>
#import <collections.h>

@interface OutputStream_c: CreateDrop_s <OutputStream>
{
@public
  FILE *fileStream;
  id <List> exprStack;
  id expr;
}
/*** methods in OutputStream_c (inserted from .m file by m2h) ***/
+ createBegin: aZone;
- setFileStream: (FILE *)file;
- setExprFlag: (BOOL)exprFlag;
- createEnd;
+ create: aZone setFileStream: (FILE *)file;
- (FILE *)getFileStream;
- getExpr;
- (void)catC: (const char *)cstring;
- (void)catLiteral: (const char *)cstring;
- (void)catBoolean: (BOOL)bool;
- (void)catChar: (char)ch;
- (void)catFloat: (float)flt;
- (void)catDouble: (double)dbl;
- (void)catLongDouble: (long double)ldbl;
- (void)catInt: (int)i;
- (void)catUnsigned: (unsigned)un;
- (void)catShort: (short)sht;
- (void)catUnsignedShort: (unsigned short)usht;
- (void)catLong: (long)lng;
- (void)catUnsignedLong: (unsigned long)ulng;
- (void)catLongLong: (long long)lnglng;
- (void)catUnsignedLongLong: (unsigned long long)ulnglng;
- (void)catPointer: (void *)ptr;
- (void)catStartExpr;
- (void)catEndExpr;
- (void)catKeyword: (const char *)keyword;
- (void)catSymbol: (const char *)symbol;
- (void)catString: (const char *)str;
- (void)catSeparator;
- (void)catArrayRank: (unsigned)rank;
- (void)catEndArray;
- (void)catArrayType: (const char *)type;
- (void)catType: (const char *)type;
- (void)catClass: (Class)class;
- (void)catStartFunction: (const char *)functionName;
- (void)catEndFunction;
- (void)catStartCons;
- (void)catEndCons;
- (void)catStartList;
- (void)catEndList;
- (void)catStartQuotedList;
- (void)catEndQuotedList;
- (void)catStartParse;
- (void)catEndParse;
- (void)catStartMakeInstance: (const char *)typeName;
- (void)catEndMakeInstance;
- (void)catStartMakeClass: (const char *)className;
- (void)catEndMakeClass;
- (void)catUnsignedPair: (unsigned)a : (unsigned)b;
@end

