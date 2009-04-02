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
Name:         InputStream.m
Description:  character string object   
Library:      collections
*/

#import <collections.h>
#import <collections/StringObject.h> // setLiteralFlag:
#import <collections/OutputStream.h>
#import <collections/InputStream.h>
#import <collections/Stream.h>
#import <defobj/defalloc.h> // getZone
#import <defobj/swarm-objc-api.h> // type definitions
#include <misc.h> // errno, fputs, isDigit, isSpace
#include <collections/predicates.h>
#include "internal.h"

@implementation InputStream_c

PHASE(Creating)

+ createBegin: aZone
{
  InputStream_c *newStream = [aZone allocIVars: self];
  return newStream;
}

- setFileStream: (FILE *)file
{
  fileStream = file;
  return self;
}

- setExpr: theExpr
{
  expr = theExpr;
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
  InputStream_c *newStream;

  newStream = [aZone allocIVars: getNextPhase (self)];
  newStream->fileStream = file;
  newStream->expr = nil;
  return newStream;
}

+ create: aZone setExpr: theExpr
{
  InputStream_c *newStream;

  newStream = [aZone allocIVars: getNextPhase (self)];
  newStream->fileStream = NULL;
  newStream->expr = theExpr;
  return newStream;
}

PHASE(Using)

- (FILE *)getFileStream
{
  return fileStream;
}

static id
readString (id inStream, char terminator)
{
  int c;
  id string;
  FILE *fp = [inStream getFileStream];
  char buf[2];

  string = [String createBegin: [inStream getZone]];
  [string setC: ""];
  [string setLiteralFlag: terminator ? YES: NO];
  string = [string createEnd];
  
  buf[1] = '\0';
  while ((c = fgetc (fp)) != EOF
         && !(terminator
              ? c == terminator
              : (isSpace (c) || c == '(' || c == ')')))
    {
      buf[0] = c;
      [string catC: buf];
    }
  if (c == EOF)
    {
      [string drop];
      return nil;
    }
  else if (!terminator)
    ungetc (c, fp);
  return string;
}

- (void)_unexpectedEOF_
{
  raiseEvent (InvalidArgument, "unexpected EOF");
}

- (void)_badType_ : obj
{
  raiseEvent (InvalidArgument, "wrong type");
}
  
- getExpr
{
  if (fileStream)
    {
      int c;
      id aZone = getZone (self);
      
      while (((c = fgetc (fileStream)) != EOF) && isSpace (c));
      if (c == EOF)
        return nil;
      else if (c == '\'')
        return [[[ArchiverQuoted createBegin: aZone]
                  setQuotedObject: [self getExpr]]
                 createEnd];
      else if (c == ':')
        {
          id newObj = readString (self, '\0');
      
          if (newObj == nil)
            [self _unexpectedEOF_];
      
          return [[[ArchiverKeyword createBegin: aZone]
                    setKeywordName: [newObj getC]]
                   createEnd];
        }
      else if (c == '<')
        {
          id newObj = readString (self, '>');
          id value;
      
          if (newObj == nil)
            [self _unexpectedEOF_];
      
          value = [[[ArchiverValue createBegin: aZone]
                     setClass: swarm_objc_lookupClass ([newObj getC])]
                    createEnd];
          [newObj drop];
          return value;
        }
      else if (c == '#')
        {
          int c2 = fgetc (fileStream);

          if (c2 == ':')
            {
              id newObj = readString (self, '\0');
          
              if (newObj == nil)
                [self _unexpectedEOF_];
          
              return [[[ArchiverKeyword createBegin: aZone]
                        setKeywordName: [newObj getC]]
                       createEnd];
            }
          else if ((c2 >= '0' && c2 <= '9') || c2 == '(')
            {
              unsigned rank;

	      ungetc (c2, fileStream);
	      if (c2 == '(')
		rank = 1;
	      else
		{
		  int ret = fscanf (fileStream, "%u", &rank);
		  
		  if (ret != 1)
		    raiseEvent (InvalidArgument,
				"Unable to scan array dimensions [ret = %d]", ret);
		}
	      
              {
                id newObj = [self getExpr];
            
                if (newObj == nil)
                  [self _unexpectedEOF_];
            
                return [[(id <ArchiverArray>)[ArchiverArray createBegin: aZone]
                          setArray: newObj]
                         createEnd];
              }
            }
          else if (c2 == '\\')
            {
              unsigned val;
              unsigned char c3;

              c3 = fgetc (fileStream);

              if (c3 >= '0' && c3 <= '3')
                {
                  int ret;

                  ungetc (c3, fileStream);
                  ret = fscanf (fileStream, "%o", &val);
              
                  if (ret != 1)
                    raiseEvent (InvalidArgument,
                                "Unable to scan octal character value");
                  c3 = (unsigned char)val;
                }
              return [[[ArchiverValue createBegin: aZone]
                        setChar: c3] createEnd];
            }
          else if (c2 == 't' || c2 == 'f')
            return [[[ArchiverValue createBegin: aZone]
                      setBoolean: (c2 == 't')] createEnd];
          else
            raiseEvent (InvalidArgument, "Unknown `#' form");
        }
      else if (c == '(')
        {
          id list = [ArchiverList create: aZone];
      
          while (YES)
            {
              id newObj = [self getExpr];
          
              if (newObj == nil)
                [self _unexpectedEOF_];
              if (ARCHIVEREOLP (newObj))
                break;
              [list addLast: newObj];
            }

          if ([list getCount] == 3 && ARCHIVERDOTP ([list atOffset: 1]))
            {
              id pair = [ArchiverPair createBegin: aZone];
          
              [pair setCar: [list getFirst]];
              [pair setCdr: [list getLast]];
              [pair setConsFormatFlag: NO];
              pair = [pair createEnd];
              [list drop];
              return pair;
            }
          else if (quote_literal_p ([list getFirst]))
            {
              id quotedObject = [ArchiverQuoted createBegin: aZone];

              [quotedObject setQuotedObject: [list atOffset: 1]];
              [list drop];
              return [quotedObject createEnd];
            }
          else if (cons_literal_p ([list getFirst]))
            {
              id pair;
          
              if ([list getCount] != 3)
                raiseEvent (InvalidArgument, "cons accepts only two arguments");
          
              pair = [ArchiverPair createBegin: aZone];
          
              [pair setCar: [list atOffset: 1]];
              [pair setCdr: [list atOffset: 2]];
              [pair setConsFormatFlag: YES];
              pair = [pair createEnd];
              [list drop];
              return pair;
            }
          return list;
        }
      else if (c == ')')
        return ArchiverEOL;
      else if (c == '.')
        return ArchiverDot;
      else if (c == '"')
        {
          id string = readString (self, '"');

          if (string)
            return string;
          else
            [self _badType_ : string];
        }
      else if (c == ';')  // Lisp comment
        {
          while (YES)  // suck up the rest of line
            {
              c = fgetc (fileStream);
              if (c == '\n')            // end of comment (marked by end of line)
                return [self getExpr];  // re-commence search 
              else if (c == EOF)
                return nil;
            } 
        }
      else
        {
          id string;
          BOOL isNumeric = YES;
          char type = _C_LNG_LNG;

          ungetc (c, fileStream);
          string = readString (self, '\0');

          {
            const char *str = [string getC];
            size_t len = strlen (str);
            size_t pos;
        
            for (pos = 0; pos < len; pos++)
              {
                char ch = str[pos];
            
                if (ch == '.')
                  type = _C_DBL;
                else if (!isDigit (ch) && !(pos == 0 && ch == '-'))
                  {
                    if (pos == len - 2)
                      {
                        if (ch == 'F')
                          type = _C_FLT;
                        else if (ch == 'D')
                          type = _C_DBL;
                        else if (ch == 'L')
                          type = _C_LNG_DBL;
                        else
                          {
                            isNumeric = NO;
                            break;
                          }
                      }
                    else
                      {
                        isNumeric = NO;
                        break;
                      }
                  }
              }
      
            if (isNumeric)
              {
                id number = [ArchiverValue createBegin: aZone];
            
                if (type == _C_LNG_DBL || type == _C_DBL || type == _C_FLT)
                  {
                    double val;
                
                    errno = 0;
                    val = strtod (str, NULL);
                    if (errno != 0)
                      raiseEvent (InvalidArgument, "Could not convert to double");
                    if (type == _C_FLT)
                      [number setFloat: (float) val];
                    else if (type == _C_DBL)
                      [number setDouble: val];
                    else if (type == _C_LNG_DBL)
                      [number setLongDouble: (long double) val];
                    else
                      abort ();
                  }
                else if (type == _C_LNG_LNG)
                  {
                    long long val;

                    errno = 0;
                    val = strtoll (str, NULL, 10);
                    if (errno != 0)
                      raiseEvent (WarningMessage, "Could not convert to long");
                    [number setLongLong: val];
                  }
                else
                  abort ();
                return [number createEnd];
              }
            else if (string)
              {
                const char *str = [string getC];

                if (str[len - 1] == ':')
                  {
                    char buf[len - 1 + 1];
                    
                    strncpy (buf, str, len - 1);
                    buf[len - 1] = '\0';
                    
                    return [[[ArchiverKeyword createBegin: aZone]
                              setKeywordName: buf]
                             createEnd];
                  }
                else if (strcmp (str, "nil") == 0)
                  return [[[ArchiverValue createBegin: aZone]
                            setNil]
                           createEnd];
                return string;
              }
            else
              [self _badType_ : string];
          }
        }
      raiseEvent (LoadError, "Unexpected character `%c'\n");
      return nil;
    }
  else
    return expr;
}
@end

