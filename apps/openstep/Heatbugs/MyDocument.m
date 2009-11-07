//
//  HeatbugsDocument.m
//  Heatbugs Swarm Application
//
//  Created by Scott Christley on 4/17/09.
//  Copyright __MyCompanyName__ 2009 . All rights reserved.
//

#import "MyDocument.h"
#import "HeatbugsGUI.h"
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
	return [HeatbugsGUI class];
}

- (NSString *)windowNibName
{
    // Override returning the nib file name of the document
    // If you need to use a subclass of NSWindowController or if your document supports multiple NSWindowControllers, you should remove this method and override -makeWindowControllers instead.
    return @"MyDocument";
}

- (void)windowControllerDidLoadNib:(NSWindowController *) aController
{
    [super windowControllerDidLoadNib:aController];
    // Add any code here that needs to be executed once the windowController has loaded the document's window.
}

- (void)attachDisplays
{
	// Here is where you attach any displays from the GUI model to actual graphical views like Raster.
	[heatbugView addDisplay: [[self getSwarm] heatDisplay]];
	[heatbugView addDisplay: [[self getSwarm] heatbugDisplay]];
}

- (void)swarmHasUpdated: (id)sender
{
	// When the GUI model has updated the displays, it informs us so we can redraw our views
	//[heatbugView display];
	[heatbugView setNeedsDisplay: YES];
}

@end
