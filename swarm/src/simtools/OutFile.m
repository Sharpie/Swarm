// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.


#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#import <stdio.h>
#import <simtools/OutFile.h>

@implementation OutFile

+create: aZone withName: (char *) theName {
  FILE *aFile ;
  id anObj ;

  aFile = fopen(theName,"w") ;
  if(aFile == NULL){
    fprintf(stderr,
      "Unable to open %s as an OutFile object!\n",theName) ;
    return nil ;
  }

  anObj = [OutFile create: aZone] ;
  [anObj _setFile_: aFile] ;
  
  return anObj ;
}

-_setFile_: (FILE *) aFile {
  theFile = aFile ;
  return self ;
}

-putString: (char *) aString {

  fprintf(theFile,"%s",aString)  ;
  return self ;
}

-putInt: (int) anInt {

  fprintf(theFile,"%d",anInt) ;
  return self ;
}

-putUnsigned: (unsigned) anUnsigned {

  fprintf(theFile,"%u",anUnsigned) ;
  return self ;
}

-putDouble: (double) aDouble {

  fprintf(theFile,"%f",aDouble) ;
  return self ;
}

-putFloat: (float) aFloat {

  fprintf(theFile,"%f",aFloat) ;
  return self ;
}

-putChar: (char) aChar {

  fprintf(theFile,"%c",aChar) ;
  return self ;  
}

-putNewLine {

  fprintf(theFile,"\n") ;
  return self ;
}

-putTab {

  fprintf(theFile,"\t") ;
  return self ;
}

-(void) drop {
  fclose(theFile) ;
  [super drop] ;
}

@end
