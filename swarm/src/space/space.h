// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

//D: The Swarm Space library is the beginnings of a library to assist in
//D: building environments for interacting agents. In general, environments
//D: can be just as varied as the agents themselves (in one view, the
//D: environment itself is simply another agent). However, many simulations
//D: have similar types of environments that can be helpfully supported by
//D: generic code.

//D: The current space library only addresses simple kinds of discretized
//D: 2d space. Improvement is planned in the future: see the todo list for
//D: ideas. Briefly, coordinates need to be elevated to the status of
//D: objects, which should hopefully allow spaces of different scales and
//D: boundary conditions to interact through a common reference system. In
//D: addition, other types of spaces are desired: continuous coordinates,
//D: other dimensions, arbitrary graphs, etc.

#import <space/Discrete2d.h>
#import <space/DblBuffer2d.h>
#import <space/Ca2d.h>
#import <space/Value2dDisplay.h>
#import <space/ConwayLife2d.h>
#import <space/Diffuse2d.h>
#import <space/Grid2d.h>
#import <space/Object2dDisplay.h>
#import <space/Int2dFiler.h>
