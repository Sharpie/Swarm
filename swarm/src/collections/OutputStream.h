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
Name:        OutputStream.h
Description: character string object
Library:     collections
*/

#import <Swarm/Create.h>
#import <Swarm/collections.h>

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
- (void)catBoolean: (BOOL)bool_;
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
- (void)catClass: (Class)class_;
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
- (void)catNil;
@end

