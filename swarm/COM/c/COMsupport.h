#include "nsISupports.h"
#include "nsIInterfaceInfo.h"
#include <defobj.h>

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
BOOL selectorIsVoidReturn (COMselector cSel);
BOOL selectorIsBooleanReturn (COMselector cSel);
const char *selectorName (COMselector cSel);
unsigned selectorArgCount (COMselector cSel);
fcall_type_t selectorArgFcallType (COMselector cSel, unsigned argIndex);
void selectorInvoke (COMselector cSel, void *args);

void *createArgVector (unsigned size);
void setArg (void *args, unsigned pos, fcall_type_t type, types_t *value);
void setReturn (void *args, unsigned pos, fcall_type_t type, void *value);
void freeArgVector (void *args);

}
