// Copyright (C) 1999 Santa Fe Institute.  This library is distributed
// without any warranty; without even the implied warranty of
// merchantability or fitness for a particular purpose.  See file
// COPYING for details and terms of copying.

#import <objectbase/SwarmObject.h>

@interface TestObject : SwarmObject 
{
  int var1;
  int var2;
  char char1;
}
- (int)getVar1;
- (int)getVar2;
- (char)getChar1;
- method1: (int)a1;
- method2: (int)a2;
- method3: (char)a3;
- printObject;
@end



