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

#import <Swarm/Create.h>
#import <Swarm/defobj.h>
#import <Swarm/collections.h>

#import <Swarm/swarmconfig.h>

#define ARCHIVER_FUNCTION_NAME "archiver"

const char *defaultPath (const char *swarmArchiver);
const char *defaultAppPath (const char *appDataPath, const char *appName,
                            const char *suffix);

@interface Archiver_c: CreateDrop_s <Archiver>
{
  id currentApplicationKey;
  id <Map> applicationMap;
  BOOL inhibitLoadFlag;
  BOOL systemArchiverFlag;
  const char *path;
@public
  id <List> classes;
  id <List> instances;
}
+ createBegin: aZone;
+ create: aZone setPath: (const char *)thePath;
- createEnd;

- setInhibitLoadFlag: (BOOL)inhibitLoadFlag;
- setPath: (const char *)path;
- setSystemArchiverFlag: (BOOL)systemArchiverFlag;
- setDefaultPath;
- setDefaultAppPath;

- createAppKey: (const char *)appName mode: (const char *)modeName;

- getApplication;

- (void)registerClient: client;
- (void)unregisterClient: client;

- (void)updateArchiver;
- (void)sync;

- getObject: (const char *)key;
- getWithZone: aZone key: (const char *)key;
- (void)putDeep: (const char *)key object: object;
- (void)putShallow: (const char *)key object: object;

- (void)drop;

@end

