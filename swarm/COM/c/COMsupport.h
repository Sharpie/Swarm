extern "C" {

void *createComponent (COMclass cClass);
void *findComponent (const char *className);
const char *copyString (const char *str);
const char *getName (COMobject cObj);

}
