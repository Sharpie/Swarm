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

#import <swarm_xmlrpc/SwarmXMLRPCServer.h>
#include <objc/Protocol.h>
#include "xmlrpc_private.h"
#import <collections.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <assert.h>

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#define SOCKET_ERROR -1
#define INVALID_SOCKET -1


@interface IncommingClient : SwarmObject {
@public
  int socket;
  XMLRPC_REQUEST request;
  int buff_size;
}

+ create: (id <Zone>)aZone withSocket: (int)aSocket;
- readMessage;
- answer: (XMLRPC_REQUEST)response;
- (void)drop;
@end

@implementation IncommingClient

+ create: (id <Zone>)aZone withSocket: (int)aSocket;
{
  socklen_t tmp = sizeof(int);

  IncommingClient* s = [super create: aZone];
  s->socket = aSocket;

  // choose optimal size for reading
  if (getsockopt(s->socket, SOL_SOCKET, SO_RCVBUF, &s->buff_size, &tmp) < 0)
    {
      raiseEvent(WarningMessage, "Could not get recieve buffer size. Set to 4096 (%s)", strerror(errno));
    }
  else
    s->buff_size = 4096;

  return s;
}

- readMessage
{
  id <String> message = [String create: globalZone];
  int n = -1, len = 0;
  char* buff = (char*)malloc(buff_size);

/*    while ((n = recv(s->socket, buff, buff_size, 0)) > 0)
      {
        [message catC: buff];
        len += n;
      }*/

  while (n <= 0)
    {
      n = recv(socket, buff, 2048, 0);

      if (n > 0)
        {
          [message catC: buff];
          len += n;
        }
    }

  request = XMLRPC_REQUEST_FromXML((const char*)[message getC], len, NULL);
  return self;
}

- answer: (XMLRPC_REQUEST)response
{
  int buff_length;
  char *buff, *outbuf;

  outbuf = XMLRPC_REQUEST_ToXML(response, &buff_length);
  buff = outbuf;

/*  for (sended_len = 0; sended_len < buff_length; )
    {
      if (sended_len + snd_buff_size < buff_length)
        sended_len += write(socket, buff, snd_buff_size);
      else
        sended_len += write(socket, buff, buff_length - sended_len + 1);
    }
*/

  send(socket, buff, strlen(buff), 0);
  return self;
}

- (void)drop
{
  close(socket);
  [super drop];
}
@end

struct thread_arg
{
    XMLRPC_SERVER xmlrpc_server;
    IncommingClient* iclient;
};

/**
 * System server functions:
 */

XMLRPC_VALUE swarm_GetProtocolList(XMLRPC_SERVER server, XMLRPC_REQUEST request, void* userData)
{
  XMLRPC_VALUE result = XMLRPC_CreateVector(NULL, xmlrpc_vector_array);
  XMLRPC_VALUE params = XMLRPC_RequestGetData(request);
  int n = XMLRPC_VectorSize(params);
  int i = 0;
  XMLRPC_VALUE arg = XMLRPC_VectorRewind(params);

  do
    {
      char* objname = (char*)XMLRPC_GetValueString(arg);

      if ([server->objects containsKey: (id)objname])
        {
          id obj = [server->objects at: (id)objname];

          // get list of protocols, that this object implement immediately
          struct objc_protocol_list* protocol_list = ((struct objc_class*)obj)->class_pointer->protocols;

          while (protocol_list)
            {
              int j;
              
              for (j = 0; j < protocol_list->count; j++)
                {
                  Protocol* p = protocol_list->list[j];
                  XMLRPC_VectorAppendString(result, objname, [p name], 0);
                }

              protocol_list = protocol_list->next;
            }
        }
      arg = XMLRPC_VectorNext(params);
    } 
  while (i < n);
  
  return result;
}

@implementation SwarmXMLRPCServer

- setPort: (int)aPort
{
  assert(aPort >= 0 && aPort <= 65535);
  port = aPort;
  return self;
}

- createEnd
{
  struct sockaddr_in s;
  int reuse = 1;

  xmlrpc_server = XMLRPC_ServerCreate();

  // creating socket
  sockfd = socket(PF_INET, SOCK_STREAM, PF_UNSPEC);

  if (sockfd == INVALID_SOCKET)
    {
      raiseEvent(InternalError, "unable to create socket");
    }

#ifndef BROCKEN_SO_REUSEADDR
  if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, (char*)&reuse, sizeof(reuse)) < 0)
    {
      close(sockfd);
      raiseEvent(InternalError, "unable to set reuse on socket");
    }
#endif

  // binding
  bzero((void*)&s, sizeof(struct sockaddr));
  s.sin_family = AF_INET;
  s.sin_port = htons(port);
  s.sin_addr.s_addr = INADDR_ANY;

  if (bind(sockfd, (struct sockaddr*)&s, sizeof(struct sockaddr)) == SOCKET_ERROR)
    {
      close(sockfd);
      raiseEvent(InternalError, "unable to bind to port");
    }

  if (listen(sockfd, 128) == SOCKET_ERROR)
    {
      close(sockfd);
      raiseEvent(InternalError, "unable to listen the port");
    }

  client_count = 0;
//  clients_semid = semget(IPC_PRIVATE, 1, IPC_CREAT);

  XMLRPC_ServerRegisterMethod(xmlrpc_server, "GetProtocolList", swarm_GetProtocolList);
  return self;
}

- registerObject: aObj withName: (const char*)aObjName
{
  XMLRPC_ServerRegisterObject(xmlrpc_server, aObjName, aObj);
  return self;
}

- (IncommingClient*)acceptClient
{
    int acc = accept(sockfd, NULL, 0);
    if (acc < 0)
      {
        raiseEvent(InternalError, "Error while accepting message");
      }
    return [IncommingClient create: [self getZone] withSocket: acc];
}

void* clientRunLoop(void* arg)
{
  XMLRPC_SERVER xmlrpc_server = ((struct thread_arg*)arg)->xmlrpc_server;
  IncommingClient* iclient = ((struct thread_arg*)arg)->iclient;

// constructing response

  while (1)
    {
      [iclient readMessage];

      XMLRPC_REQUEST response = XMLRPC_RequestNew();
      XMLRPC_RequestSetRequestType(response, xmlrpc_request_response);

      XMLRPC_RequestSetData(response, XMLRPC_ServerCallMethod(xmlrpc_server, iclient->request, NULL));
      XMLRPC_RequestSetOutputOptions(response, XMLRPC_RequestGetOutputOptions(iclient->request));

      [iclient answer: response];
    }

  [iclient drop];
}

- (void)go
{
  while (1)
    {
      IncommingClient* iclient = [self acceptClient];
      struct thread_arg arg;
      arg.xmlrpc_server = xmlrpc_server;
      arg.iclient = iclient;
      
      if (client_count >= MAX_CLIENTS) {
/*        struct sembuf s;
        bzero((void*)&s, sizeof(s));

        semop(clients_semid, &s, 1);*/
        // TODO: do something
      }
      pthread_create(&threads[client_count++], NULL, clientRunLoop, (void*)&arg);
    }
}

- (void)drop
{
  close(sockfd);
  XMLRPC_ServerDestroy(xmlrpc_server);
  [super drop];
}
@end

