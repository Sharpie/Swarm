// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc.h>

id globalTkInterp;

int
main(int argc, char ** argv) {
  Frame * top, * child;

  initCollections();
  initTkObjc(argc, argv);
  
  top = [Frame create: globalZone];
  child = [Frame createParent: top];
  printf("%s\n%s\n", [top getWidgetName], [child getWidgetName]);

  [globalTkInterp promptAndEval];
  return 0;
}
