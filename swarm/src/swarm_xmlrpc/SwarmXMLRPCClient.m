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

#import <swarm_xmlrpc/SwarmXMLRPCClient.h>
#import <swarm_xmlrpc/SwarmXMLRPCFProxy.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <assert.h>

#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define INVALID_SOCKET -1

@implementation SwarmXMLRPCClient

PHASE(Creating)
- setServerIP: (const char*)aIP
{
  addr = (char*)aIP;
  return self;
}

- setServerPort: (int)aPort
{
  port = aPort;
  return self;
}

- createEnd
{
  struct sockaddr_in s;
  int reuse = 1;

  sockfd = socket(PF_INET, SOCK_STREAM, 0);

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

  bzero((void*)&s, sizeof(struct sockaddr));
  s.sin_family = AF_INET;
  s.sin_port = htons(port);

#ifndef HAVA_INET_ATON
  s.sin_addr.s_addr = inet_addr(addr);

  if (s.sin_addr.s_addr == INADDR_NONE)
    {
#else
  if (inet_aton(addr, &s.sin_addr) == 0)
    {
#endif
      close(sockfd);
      raiseEvent(InternalError, "unable to convert IP address into network format");
    }
  int conn = connect(sockfd, (struct sockaddr*)&s, sizeof(struct sockaddr));

  if (conn == INVALID_SOCKET)
    {
      close(sockfd);
      raiseEvent(InternalError, "unable to connect to the server: %s", strerror(errno));
    }

  proxies = [Map createBegin: [self getZone]];
  [proxies setCompareCStrings];
  proxies = [proxies createEnd];

  return self;
}

PHASE(Using)

- getProxyForObject: (const char*)objName withProtocol: (Protocol*)protocol
{
  id proxy;

  if ([proxies containsKey: (id)objName])
    {
      proxy = [proxies at: (id)objName];

      if ([proxy conformsTo: protocol])
        return proxy;
      else
        raiseEvent(InternalError, "Unable create FProxy, because object with same name and different protocol exist");
    }
      
  proxy = [[[[[SwarmXMLRPCFProxy createBegin: [self getZone]] 
                                           setObject: objName] 
                                          setConnection: self]
                              setProtocol: protocol]
                                                createEnd];

  [proxies at: (id)objName insert: proxy];

  return proxy;
}

- (XMLRPC_VALUE) sendMessage: (const char*)message
{
  int buff_size;
  char* buff;
  socklen_t tmp = sizeof(int);

  // choose optimal size for reading
  if (getsockopt(sockfd, SOL_SOCKET, SO_RCVBUF, &buff_size, &tmp) < 0)
    {
      raiseEvent(WarningMessage, "Could not get recieve buffer size. Set to 4096 (%s)", strerror(errno));
    }


  // sending message to the server
  send(sockfd, (void*)message, strlen(message), 0);

  { // recieving answer
    id <String> answer = [String create: globalZone];
    buff = malloc(buff_size);
    int n, len = 0;

    // read message into buffer
    //while ((n = recv(sockfd, buff, buff_size, 0)) > 0)
/*    while ((n = fread(buff, buff_size, sizeof(char), fp)) > 0)
      {
        [answer catC: buff];
        len += n;
      }*/
    n = recv(sockfd, buff, 2048, 0);
    [answer catC: buff];
    len += n;
printf("%s\n", buff);
    if (n < 0)
      {
        raiseEvent(WarningMessage, "Coult not get answer for message because: %s", strerror(errno));
        return NULL;
      }

    // make returned value as result of this function
    XMLRPC_REQUEST response = XMLRPC_REQUEST_FromXML((const char*)[answer getC], len, NULL);
    return XMLRPC_RequestGetData(response);
  }
}

@end

