// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <objc/objc.h>

void tkobjc_dragAndDrop (id source, id object);
void tkobjc_dragAndDropArg (id source, id object, int arg);
void tkobjc_dragAndDropTarget (id target, id object);

int tkobjc_doOneEventSync (void);
int tkobjc_doOneEventAsync (void);

void tkobjc_bindButton3ToSpawn (id widget, id self, int focusFlag);
void tkobjc_bindButton3ToArgSpawn (id widget, id self, int which);

void tkobjc_ringBell (void);

id tkobjc_drag_and_drop_object (void);

void tkobjc_configureSpecialBitmap (id widget);

void tkobjc_update (void);
void tkobjc_releaseAndUpdate (void);
void tkobjc_updateIdleTasks (int hold);

const char *tkobjc_dynamicEval (const char *cmd);

void tkobjc_focus (id widget);

void tkobjc_makeFrame (id widget);
void tkobjc_pack (id widget);

const char *tkobjc_createText (id widget, int x, int y, const char *text, const char *font, BOOL centerFlag);
