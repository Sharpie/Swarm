// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "TestObject.h"

@implementation TestObject

+createBegin: (id) aZone {
  TestObject * obj;
  
  obj = [super createBegin: aZone];

  obj->var1 = 1;
  obj->var2 = 1;
  obj->char1 = 'a' ;

  return obj;
}

-method1: (int) a1 {
  var1 = a1*var1;
  return self;
}

-method2: (int) a2 {
  var2 = var2 + a2;
  return self;
}

-method3: (char) a3 {
  char1 = a3;
  return self;
}


-printObject {
  
  printf("var1 = %d\n", var1);
  printf("var2 = %d\n", var2);
  printf("char = %c\n", char1);

  return self;
}

@end







