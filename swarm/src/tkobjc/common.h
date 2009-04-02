// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <defobj/swarm-objc-api.h>

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

const char *tkobjc_createText (id aZone, id widget, int x, int y, const char *text, const char *font, BOOL centerFlag);
