#include "nsISupports.h"
#include "nsIInterfaceInfo.h"
#include <objc/objc.h>
#include <defobj.h>
#include "jsapi.h"
#include <swarmITyping.h>

PRBool findMethod (nsISupports *target, const char *methodName,
                   nsISupports **interface, PRUint16 *index, const nsXPTMethodInfo **methodInfo);

void printGetters (nsISupports *obj);

extern "C" {
#include "../../src/defobj/COM.h"
}

void *createComponent (COMclass cClass);
void *findComponent (const char *className);
const char *copyString (const char *str);
const char *getName (COMobject cObj);
const char *getComponentName (COMclass cClass);

COMobject normalize (COMobject cObj);

COMobject selectorQuery (COMselector cObj);
BOOL selectorIsJavaScript (COMselector cSel);
BOOL selectorIsVoidReturn (COMselector cSel);
BOOL selectorIsBooleanReturn (COMselector cSel);
const char *selectorName (COMselector cSel);
unsigned selectorArgCount (COMselector cSel);
fcall_type_t selectorArgFcallType (COMselector cSel, unsigned argIndex);
void selectorCOMInvoke (COMselector cSel, void *args);
void selectorJSInvoke (COMselector cSel, void *args);

void *COMcreateArgVector (unsigned size);
void COMsetArg (void *args, unsigned pos, fcall_type_t type, types_t *value);
void COMsetReturn (void *args, unsigned pos, fcall_type_t type, types_t *value);
void COMfreeArgVector (void *args);

void COMcollectMethods (COMclass cClass, COM_collect_method_func_t func, BOOL gettersFlag);

JSContext *currentJSContext ();
void *JScreateArgVector (unsigned size);
void JSsetArg (void *args, unsigned pos, fcall_type_t type, types_t *value);
void JSsetReturn (void *args, unsigned pos, fcall_type_t type, types_t *value);
void JSfreeArgVector (void *args);

swarmITyping *COM_objc_ensure_object_COM (id oObject);
nsresult COM_objc_ensure_object_COM_return (id oObject, const nsIID *iid, void **ret);
swarmITyping *COM_add_object_COM (swarmITyping *cObject, id oObject);


#define SD_COM_ENSURE_OBJECT_COM(oObject) COM_objc_ensure_object_COM (oObject)
#define SD_COM_ENSURE_OBJECT_COM_RETURN(oObject,type) COM_objc_ensure_object_COM_return (oObject, &NS_GET_IID (type), (void **) ret)
#define SD_COM_ENSURE_THIS_OBJECT_OBJC() SD_COM_ENSURE_OBJECT_OBJC(NS_STATIC_CAST(swarmITyping*,this))
#define SD_COM_ADD_THIS_OBJECT_COM(oObject) COM_add_object_COM (NS_STATIC_CAST(swarmITyping*,this),oObject)
#define SD_COM_UPDATE_PHASE_RETURN(oObject, type) *ret = NS_STATIC_CAST (type, swarm_directory_update_phase_COM (oObject))
