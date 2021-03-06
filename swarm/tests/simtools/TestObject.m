// Swarm library. Copyright (C) 1999, 2000 Swarm Development Group.  This library
// is distributed without any warranty; without even the implied
// warranty of merchantability or fitness for a particular purpose.
// See file COPYING for details and terms of copying.

#import "TestObject.h"

@implementation TestObject

+ createBegin: aZone 
{
  TestObject * obj;
  
  obj = [super createBegin: aZone];

  obj->var1 = 1;
  obj->var2 = 1;
  obj->char1 = 'a' ;

  return obj;
}

- (int)getVar1
{
  return var1;
}

- (int)getVar2
{
  return var2;
}

- (char)getChar1
{
  return char1;
}

- method1: (int)a1 
{
  var1 = a1*var1;
  return self;
}

- method2: (int)a2 
{
  var2 = var2 + a2;
  return self;
}

- method3: (char)a3 
{
  char1 = a3;
  return self;
}


- printObject 
{
  printf("var1 = %d\n", var1);
  printf("var2 = %d\n", var2);
  printf("char = %c\n", char1);

  return self;
}

@end







