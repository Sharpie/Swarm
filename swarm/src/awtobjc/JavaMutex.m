// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <javaobjc/global.h>
#import <javaobjc/JavaMutex.h>

@implementation JavaMutex;

+ new {

#if 0
  // mutexobj unused? -mgd

  // look up our class id
  id jMutexClass = [JavaClass new: "JavaJMutex"];

  // call the void constructor
  jobject mutexobj = [jMutexClass construct];
#endif

  self = [JavaMutex alloc];

#if 0
// huh? -mgd
  // establish the JavaObject of this class backed by this real object
  [super init: jRasterClass : mutex];
#endif

  return self;
}

- free
{
  [super free];
  return self;
}

// take the lock
- take
{
  if (_take == 0)
    _take = [self findMethod: "take" signature: "()V"];
  
  [self callVoidMethod: _take];
  return self;
}

// drop the lock
- drop
{
  if (_drop == 0)
    _drop = [self findMethod: "drop" signature: "()V"];

  [self callVoidMethod: _drop];
  return self;
}

@end

