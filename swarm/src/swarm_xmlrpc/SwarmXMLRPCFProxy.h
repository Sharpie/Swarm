#import <objectbase/SwarmObject.h>
#include <objc/objc-api.h>
#import <swarm_xmlrpc/SwarmXMLRPCClient.h>

@interface SwarmXMLRPCFProxy : SwarmObject {
@private
    char* objName;
    SwarmXMLRPCClient* connection;
    Protocol* _protocol;
}

- setProtocol: (Protocol*)protocol;
- setObject: (const char*)name;
- setConnection: aConn;
- (char*)getName;

- (retval_t)forward: (SEL)aSel :(arglist_t)argFrame;

@end
