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
BOOL selectorIsVoidReturn (COMobject cSel);
BOOL selectorIsBooleanReturn (COMobject cSel);
const char *selectorName (COMobject cSel);
unsigned selectorArgCount (COMobject cSel);
fcall_type_t selectorArgFcallType (COMobject cSel, unsigned argIndex);
void *createArgVector (unsigned size);
void addArg (fcall_type_t type, void *value);
void setReturn (fcall_type_t type, void *value);
}
