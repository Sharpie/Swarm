// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#import <stdio.h>
#import <simtools/InFile.h>

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

-(int) getInt: (int *) anInt {
  int retVal ;

  retVal = fscanf(theFile,"%d",anInt) ;
  if(retVal == EOF)
    return 0 ;
  else 
    return 1 ;
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
