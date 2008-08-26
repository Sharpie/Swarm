#import <swarm_xmlrpc.h>
#import <swarm_xmlrpc/SwarmXMLRPCFProxy.h>
#include <xmlrpc.h>
#include <objc/mframe.h>

@implementation SwarmXMLRPCFProxy

PHASE(Setting)
- setObject: (const char*)name
{
  objName = (char*)name;
  return self;
}

- setProtocol: (Protocol*)protocol
{
  _protocol = protocol;
  return self;
}

- setConnection: aConn
{
  connection = aConn;
  return self;
}

PHASE(Using)
-(retval_t)forward: (SEL)aSel :(arglist_t)argFrame
{
  XMLRPC_VALUE xParamList;
  XMLRPC_REQUEST request = XMLRPC_RequestNew();

  xParamList = XMLRPC_CreateVector(NULL, xmlrpc_vector_array);

  NSArgumentInfo info;
  types_t val;

  struct objc_method_description* s = [_protocol descriptionForInstanceMethod: aSel];
  if (!s)
    [self doesNotRecognize: aSel];

  char* type = s->types;

  type = (char*)mframe_next_arg (type, &info);  
  mframe_get_arg (argFrame, &info, &val);

  // skip object and selector
  type = (char*)mframe_next_arg (type, &info);
  type = (char*)mframe_next_arg (type, &info);

  while ((type = (char*)mframe_next_arg (type, &info)))
    {
      mframe_get_arg (argFrame, &info, &val);

      switch (*info.type)
        {
   	    case _C_UCHR:
          XMLRPC_VectorAppendBoolean(xParamList, NULL, val.boolean);
          break;
        case _C_INT:
          XMLRPC_VectorAppendInt(xParamList, NULL, val.sint);
          break;
        case _C_DBL:
          XMLRPC_VectorAppendDouble(xParamList, NULL, val._double);
          break;
        case _C_CHARPTR:
          XMLRPC_VectorAppendString(xParamList, NULL, val.string, 0);
          break;
	    case _C_ID:
	      {
	        XMLRPC_VALUE xArg = XMLRPC_CreateVector(NULL, xmlrpc_vector_array);
	        if ([val.object getClass] == [self getClass])
	          { // Just say server, that we pass internal object
		        XMLRPC_VectorAppendString(xArg, NULL, "[!]", 0);
		        XMLRPC_VectorAppendString(xArg, NULL, [val.object getName], 0);
	          }
	        else
	          {
		        raiseEvent(InternalError, "Sending of object in RPC argument does not supported");
	          }

   	        XMLRPC_AddValueToVector(xParamList, xArg);
	      }
	      break;
	    /*	case _C_CLASS:
	  break;
	case _C_SEL:
	break; */

        default:
          raiseEvent(InternalError, "type %c does not supported by XML-RPC specificatoin", *info.type);
          break;
        }
    }

  char* selname = (char*)sel_get_name(aSel);
  char* result = malloc(strlen(objName) + strlen(selname) + 2);
  sprintf(result, "%s.%s", objName, selname);

  XMLRPC_RequestSetRequestType(request, xmlrpc_request_call);
  XMLRPC_RequestSetMethodName(request, result);

  XMLRPC_RequestSetData(request, xParamList);

  { // send request over connection to the server
    char *buff = XMLRPC_REQUEST_ToXML(request, 0);
    puts(buff);

    XMLRPC_VALUE xResult = [connection sendMessage: buff];
    switch (XMLRPC_GetValueTypeEasy(xResult))
      {
      case xmlrpc_none:
      case xmlrpc_empty:
        return NULL;
      case xmlrpc_int:
        {
          int* res = (int*)malloc(sizeof(int));
          *res = XMLRPC_GetValueInt(xResult);
          return (retval_t)res; 
        }
      case xmlrpc_double:
        // !error. not supported because of different sizes of int and double
        return nil;
      case xmlrpc_boolean:
        // !error. not supported becase of different sizes of int and boolean
        return nil;
      case xmlrpc_string:
        {
          char** res = (char**)malloc(sizeof(char*));
          *res = (char*)XMLRPC_GetValueString(xResult);
          return (retval_t)res;
        }
      case xmlrpc_vector: // return proxy for object
        {
          XMLRPC_VALUE xArg = XMLRPC_VectorRewind(xResult);
          Protocol* protocol;
          char *object_name, *protocol_name;
          id* q;

          if (XMLRPC_GetValueType(xArg) != xmlrpc_string)
            {
              raiseEvent(WarningMessage, "Invalid format for object in result");
              return nil;
            }
          if (strcmp(XMLRPC_GetValueString(xArg), "[!]"))
            {
              raiseEvent(WarningMessage, "Invalid format for object in result");
              return nil;
            }

          xArg = XMLRPC_VectorNext(xResult);
          if (XMLRPC_GetValueType(xArg) != xmlrpc_string)
            { 
              raiseEvent(WarningMessage, "Invalid format for object in result");
              return nil;
            }
          object_name = (char*)XMLRPC_GetValueString(xArg);

          xArg = XMLRPC_VectorNext(xResult);
          if (XMLRPC_GetValueType(xArg) != xmlrpc_string)
            {
              raiseEvent(WarningMessage, "Invalid format for object in result");
              return nil;
            }
          protocol_name = (char*)XMLRPC_GetValueString(xArg);
          protocol = findProtocol(protocol_name);

          if (!protocol)
            {
              raiseEvent(WarningMessage, "Unknown protocol for object in result: `%s'", protocol_name);
              return nil;
            }

          q = (id*)malloc(sizeof(id));
          *q = [connection getProxyForObject: object_name withProtocol: protocol];
          return q;
        }
      default: 
        raiseEvent(WarningMessage, "Unsupported result's type of XML-RPC call");
        return nil;
      }
  }

  XMLRPC_RequestFree(request, 1);
  return self;
}

- (BOOL) conformsTo: (Protocol*)aProtocolObject
{
  if ([super conformsTo: aProtocolObject])
    return YES;
  
  if ([_protocol conformsTo: aProtocolObject])
    return YES;

  return NO;
}

- (char*)getName
{
  return SSTRDUP (objName);
}
@end
