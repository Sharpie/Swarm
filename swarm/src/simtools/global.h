// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/ProbeDisplayManager.h>
#import <swarmobject.h>
#import <random/PMMLCG.h>
#import <random/Uniform.h>

// global random number generators (defaults)
extern PMMLCG * randomGenerator;
extern Uniform * uniformRandom;

// Source of all Probe Objects in the system...
extern ProbeLibrary * probeLibrary;

// Manager that keeps track of active probes to be updated
extern ProbeDisplayManager * probeDisplayManager;

void initSwarm(int argc, char ** argv);

// Flag for whether we're in graphics mode or not. Default is 1.
extern int swarmGUIMode;
