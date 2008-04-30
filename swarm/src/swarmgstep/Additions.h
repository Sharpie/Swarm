// Swarm library. Copyright © 2004 Swarm Development Group.
//
// Author: Scott Christley
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

//
// GNUstep extensions
//
// These are methods normally defined in NSObject root class but Swarm is
// based upon Object root class so we need our own implementations.
//

#import <Foundation/NSInvocation.h>
#import <Foundation/NSMethodSignature.h>
#import <defobj/DefObject.h>

@interface Object_s (GNUstepExtensions)

- (NSMethodSignature*) methodSignatureForSelector: (SEL)aSelector;
- (void) forwardInvocation: (NSInvocation*)anInvocation;

@end
