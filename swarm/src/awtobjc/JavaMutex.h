#import <awtobjc/JavaObject.h>

//  simple mutex implemented using an underlying Java mutex
//  object

@interface JavaMutex: JavaObject
{
  jmethodID _take;
  jmethodID _drop;
}

+ new;
- free;

- take;
- drop;

@end

