#import <defobj.h>
#import "graph.sym"

void initGraphLibrary() {
  static BOOL  already_initialized = 0;

  if ( already_initialized ) return;
  already_initialized = 1;

  defsymbol( RectangleNode );
  defsymbol( OvalNode );
}

