// Swarm library. Copyright © 1996-2000 Swarm Development Group.
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

#import <tkobjc.h>
#import <objectbase/Arguments.h>

int valueOne;
char *valueTwo;

int
main (int argc, const char **argv)
{
  Histogram *h;
  double data[] = { 1.0, 3.0, 2.0, 5.0, 0.0, 6.0, 2.0, 9.0, 1.0, 4.0};
  const char *names[] = 
  {"foo", "bar", "baz", "a", "b", "longname",
   "foo", "bar", "baz", "a"};
  const char *colors[] = 
  {"black", "grey50", "red", "green", "blue", "orange", "purple", "yellow",
   "black", "grey50", "red", "green", "blue", "orange", "purple", "yellow"};
  
  initModule (collections);
  initTkObjc ([Arguments createArgc: argc Argv: argv]);
  
  h = [Histogram create: globalZone];
  [h setNumPoints: 10 Labels: names Colors: colors];
  [h drawHistogramWithDouble: data];
  [h pack];
  
  [globalTkInterp promptAndEval];
  exit (0);
}
