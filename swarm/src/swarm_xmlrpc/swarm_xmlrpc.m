#import <swarm_xmlrpc.h>
#import <collections/Map.h>
#include <objc/Protocol.h>

static id <Map> protocols;

void
initSwarmXMLRPC(void)
{
  protocols = [Map createBegin: globalZone];
  [protocols setCompareCStrings];
  protocols = [protocols createEnd];
}

void register_protocol(char* name, Protocol* protocol)
{
  [protocols at: (id)name insert: (id)protocol];
}

Protocol* findProtocol(const char* name)
{
  if ([protocols containsKey: (id)name])
    return [protocols at: (id)name];
  else
    return nil;
}

