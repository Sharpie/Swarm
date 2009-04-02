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

#import <Swarm/Archiver.h>

#import <Swarm/swarmconfig.h>

#define SWARMARCHIVER_LISP ".swarmArchiver"
#define SWARMARCHIVER_LISP_SUFFIX ".scm"

extern id lispArchiver;

@interface Application: CreateDrop
{
  const char *name;
  id <Map> streamMap;
}
+ createBegin: aZone;
- setName: (const char *)name;
- getStreamMap;
@end

@interface LispArchiver_c: Archiver_c <LispArchiver>
{
  id inStreamZone;
}
+ createBegin: aZone;
+ create: aZone setPath: (const char *)path;
- setDefaultPath;
- setDefaultAppPath;

- createEnd;
- ensureApp: appKey;

- (BOOL)_load_;
- (void)lispLoadArchiver: (id <InputStream>)stream;

- (void)sync;

- (void)putDeep: (const char *)key object: object;
- (void)putShallow: (const char *)key object: object;
- _getWithZone_: aZone key: (const char *)key;
- getObject: (const char *)key;
- getWithZone: aZone key: (const char *)key;

- (void)drop;

@end
