//
//  ÇPROJECTNAMEÈDocument.m
//  ÇPROJECTNAMEÈ Swarm Application
//
//  Created by ÇFULLUSERNAMEÈ on ÇDATEÈ.
//  Copyright ÇORGANIZATIONNAMEÈ ÇYEARÈ . All rights reserved.
//


#import "MyDocument.h"
#import "ÇPROJECTNAMEÈGUI.h"
#import <Swarm/Raster.h>

@implementation MyDocument

- (id)init
{
  self = [super init];
  if (self) {
		
    // Add your subclass-specific initialization here.
    // If an error occurs here, send a [self release] message and return nil.
    
  }
  return self;
}

- (Class)modelGUIClass
{
  // This method is called to determine what class to use for the top-level
  // Swarm GUI model when the simulation is initialized.
  return [ÇPROJECTNAMEÈGUI class];
}

- (NSString *)windowNibName
{
  // Override returning the nib file name of the document
  // If you need to use a subclass of NSWindowController
  // or if your document supports multiple NSWindowControllers,
  // you should remove this method and override -makeWindowControllers instead.
  return @"MyDocument";
}

- (void)windowControllerDidLoadNib:(NSWindowController *) aController
{
  [super windowControllerDidLoadNib:aController];

  // Add any code here that needs to be executed once the
  // windowController has loaded the document's window.

  [modelWindow makeKeyAndOrderFront: self];
}

- (void)attachDisplays
{
  // Here is where you attach any displays from the GUI model
  // to actual graphical views like Raster.
  
  // [someView addDisplay: [[self getSwarm] someDisplay]];
}

- (void)swarmHasUpdated: (id)sender
{
  // When the GUI Swarm model has updated, it informs us so we can redraw our views

  // The sender is the GUI Swarm model, so we can message it if we want to obtain
  // more explicit information about what has been updated.  This might allow us
  // to selectively re-display just some views.  In the simplest case, just
  // indicate that everything needs re-display.

  // NOTE: Most likely need to force synchronous display with -display
  // instead of asynchronous display with -setNeedsDisplay: because
  // the drawing code might need to access the current model state,
  // and we don't want this to change in the middle of drawing by
  // letting the simulation to keep running.

  // [someView display];

  // Display simulation time
  [cycleText setIntValue: [[[self getSwarm] mainModel] numberOfCycles]];
}

@end
