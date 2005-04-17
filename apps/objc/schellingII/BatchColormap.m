// Paul Johnson, copyright 2000-2004

// The code is made available under the provisions of the GNU Greater
// Public License (version 2 or newer, at the user's discretion).  That
// license is available with this package in a file called "gpl.txt".
// The code is made available without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.


// Class that can serve up a colormap to BatchRaster when
// the GUI isn't running. Modified from code by Nelson:
// Benedikt Stefansson <benedikt@ucla.edu>. First version 7/23/97

#import "BatchColormap.h"
#import "colors.m"


@implementation BatchColormap

-createEnd {
  int i;

  for (i = 0; i < MAXCOLORS; i++) 
    isSet[i] = 0;
  
  return self;
}


-(RGB *) rgbValue: (int) c {

  if ([self colorIsSet: c])
    return xcolors[c];
  else {
    [InvalidArgument raiseEvent: "attempted to access unset color %d\n", c];
    return nil;
  }

}


// set a new colormap entry to something. Error if it's already set.
-(BOOL) setColor: (Color) c ToName: (const char *) colorName {
  int i;
  int found=-1; 
 
  if ([self colorIsSet: c]) {
    [InvalidArgument raiseEvent: "attempted to set color %d twice\n", c];
    return 0;
  } else {
    RGB * xc;
    isSet[c] = 1;
    xc=[RGB create: [self getZone]];
    for(i=0;i<NUMNAMES;i++) {
      if(!strcmp(colorName,colornames[i])) {
	found=i;
	break;
      }
    }
    if(found!=-1) 
      [xc setRed: redvalue[found] Green: greenvalue[found] Blue: bluevalue[found]];
      
    xcolors[c] = xc;
    return 1;
  }
}

// allocate an RGB combo.
-(BOOL) setColor: (int) c ToRed: (double) r Green: (double) g Blue: (double) b {
  unsigned ru;
  unsigned gu;
  unsigned bu;
  RGB * xc;
  

  if ([self colorIsSet: c]) {
    return 0;
  } else {
    isSet[c] = 1;
    xc=[RGB create: [self getZone]];
    ru = r * 0xffU;
    gu = g * 0xffU;
    bu = b * 0xffU;

    [xc setRed: ru Green: gu Blue: bu];   
    xcolors[c] = xc;
    return 1;
  }
} 

// allocate grey: just a convenience.
-(BOOL) setColor: (int) c ToGrey: (double) g {
  return [self setColor: c ToRed: g Green: g Blue: g];
}


// is the colour actually set?
-(BOOL) colorIsSet: (int) c {
  return isSet[c];
}


@end


@implementation RGB

-setRed: (unsigned) r Green: (unsigned) g Blue: (unsigned) b {
  red=r;
  green=g;
  blue=b;

  return self;
}

-(int) getRed {
  return red;
}


-(int) getGreen {
  return green;
}

-(int) getBlue {
  return blue;
}

@end



