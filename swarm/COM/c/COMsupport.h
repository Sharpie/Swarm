#ifndef __COMsupport_h__
#define __COMsupport_h__
#include "nsISupports.h"
#include "nsIInterfaceInfo.h"
#include <objc/objc.h>
#include <defobj.h>
#include "jsapi.h"
#include <swarmITyping.h>

PRBool findMethod (nsISupports *target, const char *methodName,
                   const nsIID **iid, PRUint16 *index, const nsXPTMethodInfo **methodInfo);

void printGetters (nsISupports *obj);
fcall_type_t JSToFcallType (unsigned type);

struct method_value {
  const nsIID *methodIID;
  PRUint16 methodIndex;
  const nsXPTMethodInfo *methodInfo;
};

extern "C" {
#include "../../src/defobj/COM.h"
}

fcall_type_t methodParamFcallType (const nsXPTMethodInfo *methodInfo, PRUint16 paramIndex);

COMobject COMcreateComponent (COMclass cClass);
COMclass COMfindComponent (const char *className);
const char *COMcopyString (const char *str);
const char *COMgetName (COMobject cObj);
const char *COMgetComponentName (COMclass cClass);
COMclass COMgetClass (COMobject);

COMclass COMcopyComponentID (COMclass cClass);
COMobject COMnormalize (COMobject cObj);

COMobject selectorQuery (COMselector cObj);
BOOL selectorIsJavaScript (COMselector cSel);
BOOL selectorIsVoidReturn (COMselector cSel);
BOOL selectorIsBooleanReturn (COMselector cSel);
const char *selectorName (COMselector cSel);
unsigned selectorArgCount (COMselector cSel);
fcall_type_t selectorArgFcallType (COMselector cSel, unsigned argIndex);
COMselector selectorCreate (COMmethod cMethod);
COMmethod selectorMethod (COMselector cSel);

void *COMcreateParams (unsigned size);
void COMfreeParams (void *args);

void COMsetArg (void *params, unsigned pos, val_t *value);
void COMsetReturn (void *params, unsigned pos, val_t *value);

void COMcollect (COMclass cClass,
                 COM_collect_variable_func_t variableFunc, 
                 COM_collect_method_func_t methodFunc);
void JScollect (COMobject cObj,
                JS_collect_func_t variableFunc,
                JS_collect_func_t methodFunc);

JSContext *currentJSContext ();
BOOL COMisJavaScript (COMobject cObj);
void *JScreateParams (unsigned size);
void JSsetArg (void *args, unsigned pos, val_t *value);
void JSsetReturn (void *params, unsigned pos, val_t *value);
void JSfreeParams (void *args);

BOOL JSprobeVariable (COMobject cObj, const char *variableName, val_t *ret);
void JSsetVariable (COMobject cObj, const char *variableName, val_t *val);
void JSmethodInvoke (COMobject cObj, const char *methodName, void *params);
unsigned JSmethodArgCount (COMobject cObj, const char *methodName);

const char *COMmethodName (COMmethod cMethod);
unsigned COMmethodArgCount (COMmethod cMethod);
fcall_type_t COMmethodParamFcallType (COMmethod cMethod, unsigned paramIndex);
void COMmethodSetReturn (COMmethod cMethod, void *params, void *value);
void COMmethodInvoke (COMobject cObj, COMmethod cMethod, void *params);

swarmITyping *COM_objc_ensure_object_COM (id oObject);
nsresult COM_objc_ensure_object_COM_return (id oObject, const nsIID *iid, void **ret);
swarmITyping *COM_add_object_COM (swarmITyping *cObject, id oObject);


#define SD_COM_ENSURE_OBJECT_COM(oObject) COM_objc_ensure_object_COM (oObject)
#define SD_COM_ENSURE_OBJECT_COM_RETURN(oObject,type) COM_objc_ensure_object_COM_return (oObject, &NS_GET_IID (type), (void **) ret)
#define SD_COM_ENSURE_THIS_OBJECT_OBJC() SD_COM_ENSURE_OBJECT_OBJC(NS_STATIC_CAST(swarmITyping*,this))
#define SD_COM_ADD_THIS_OBJECT_COM(oObject) COM_add_object_COM (NS_STATIC_CAST(swarmITyping*,this),oObject)
#define SD_COM_UPDATE_PHASE_RETURN(oObject, type) *ret = NS_STATIC_CAST (type, swarm_directory_update_phase_COM (oObject))
#endif
