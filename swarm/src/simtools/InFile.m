// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#import <stdio.h>
#import <simtools/InFile.h>
#import <collections/String.h>

@implementation InFile

+create: aZone withName: (char *) theName {
  FILE *aFile ;
  id anObj ;

  aFile = fopen(theName,"r") ;
  if(aFile == NULL){
    fprintf(stderr,
      "Unable to open %s as an InFile object!\n",
       theName) ;
    return nil ;
  }

  anObj = [InFile create: aZone] ;
  [anObj _setFile_: aFile] ;
  
  return anObj ;
}

-_setFile_: (FILE *) aFile {
  theFile = aFile ;
  return self ;
}

-(int) getWord: (char *) aWord {
  int retVal ;

  retVal = fscanf(theFile,"%s",aWord)  ;
    if(retVal == EOF)
      return 0 ;
    else 
      return 1 ;
}

- (int)getLine: (char *)aLine 
{
  int c, pos = 0;
  
  while ((c = fgetc (theFile)) != EOF && (c == ' ' || c == '\t'));
  while (c != EOF && c != '\n')
    {
      aLine[pos++] = c;
      c = fgetc (theFile);
    }
  aLine[pos] = '\0';
  if (c == EOF)
    return 0 ;
  else
    {
      ungetc ('\n', theFile);
      return 1 ;
    }
}

-(int) getInt: (int *) anInt {
  int retVal ;

  retVal = fscanf(theFile,"%d",anInt) ;
  if(retVal == EOF)
    return 0 ;
  else 
    return 1 ;
}

-(int) getUnsigned: (unsigned *) anUnsigned {
  int retVal ;

  retVal = fscanf(theFile,"%u",anUnsigned) ;
  if(retVal == EOF)
    return 0 ;
  else 
    return 1 ;
}

-(int) getLong: (long *) aLong {
  int retVal;

  retVal = fscanf(theFile,"%ld", aLong);
  if (retVal == EOF)
    return 0;
  else
    return 1;
}

-(int) getUnsignedLong: (unsigned long *) anUnsLong {
  int retVal;

  retVal = fscanf(theFile, "%lu", anUnsLong);
  if (retVal == EOF)
    return 0;
  else
    return 1;
}

-(int) getDouble: (double *) aDouble {
  int retVal ;

  retVal = fscanf(theFile,"%lg",aDouble) ;
  if(retVal == EOF)
    return 0 ;
  else 
    return 1 ;
}

-(int) getFloat: (float *) aFloat {
  int retVal ;

  retVal = fscanf(theFile,"%g",aFloat) ;
  if(retVal == EOF)
    return 0 ;
  else 
    return 1 ;
}

-(int) getChar: (char *) aChar {
  (*aChar) = fgetc(theFile) ;
  if( (*aChar) == EOF)
    return 0 ;
  else 
    return 1 ;
}

-(int) unGetChar: (char) aChar {
  if( (ungetc(aChar,theFile)) == EOF )
    return 0 ;
  else 
    return 1 ;
}

-(int) skipLine {
  char aChar ;

  while(1){
    aChar = fgetc(theFile) ;
    if(aChar == EOF)
      return 0 ;
    if(aChar == '\n')
      return 1 ;
  }
}

-(void) drop {
  fclose(theFile) ;
  [super drop] ;
}

@end
