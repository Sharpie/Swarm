
#import <space.h>
#import <space/ConwayLife2d.h>


@interface ConwayWorld: ConwayLife2d {
  id observer;

}

- setObserver: anObject;

- swapColorAtX: (unsigned) x Y: (unsigned) y;


- eraseAll;


- stepRule;

@end

