#include "nsISupports.h"
#include "nsIInterfaceInfo.h"

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
char selectorArgObjcType (COMobject cSel, unsigned argIndex);
}
