#include "nsISupports.h"
#include "nsIInterfaceInfo.h"
#include <defobj.h>
#include "jsapi.h"

PRBool findMethod (nsISupports *target, const char *methodName,
                   nsISupports **interface, PRUint16 *index, const nsXPTMethodInfo **methodInfo);

extern "C" {
#include "../../src/defobj/COM.h"

void *createComponent (COMclass cClass);
void *findComponent (const char *className);
const char *copyString (const char *str);
const char *getName (COMobject cObj);

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

JSContext *currentJSContext ();
void *JScreateArgVector (unsigned size);
void JSsetArg (void *args, unsigned pos, fcall_type_t type, types_t *value);
void JSsetReturn (void *args, unsigned pos, fcall_type_t type, types_t *value);
void JSfreeArgVector (void *args);

}
