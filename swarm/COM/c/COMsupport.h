extern "C" {
#include "../../src/defobj/COM.h"

void *createComponent (COMclass cClass);
void *findComponent (const char *className);
void *findMethod (nsISupports *target, const char *methodName);
const char *copyString (const char *str);
const char *getName (COMobject cObj);
void addRef (COMobject cObj);

}
