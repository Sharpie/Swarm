// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc.h>
#import <objectbase/Arguments.h>

id globalTkInterp;

int
main (int argc, const char **argv)
{
  Frame *top, *child;

  initModule (collections);
  initTkObjc ([Arguments createArgc: argc Argv: argv]);
  
  top = [Frame create: globalZone];
  child = [Frame createParent: top];
  printf("%s\n%s\n", [top getWidgetName], [child getWidgetName]);

  [globalTkInterp promptAndEval];
  return 0;
}
