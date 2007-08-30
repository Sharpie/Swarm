// Swarm library. Copyright © 1996-2007 Swarm Development Group.
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

#import <swarm_xmlrpc.h>
#import <objectbase/SwarmObject.h>
#include <xmlrpc.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/sem.h>

#define MAX_CLIENTS 20

@interface SwarmXMLRPCServer : SwarmObject<SwarmXMLRPCServer> {
@private
  int port;
  int sockfd;
  XMLRPC_SERVER xmlrpc_server;

  int client_count;
  pthread_t threads[MAX_CLIENTS];
//  int clients_semid;
}

- setPort: (int)aPort;
- createEnd;
- registerObject: aObj withName: (const char*)aObjName;
- (void)go;
- (void)drop;
@end

