// Swarm library. Copyright � 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj.h>

@protocol TkExtra
- (const char *)getBltVersion;
- (const char *)getBltFullVersion;
- (BOOL)newBLTp;
- eval: (const char *)fmt, ...;
- (const char *)result;
- linkVariableInt: (const char *)variableName;
- promptAndEval;
- (const char *)variableValue: (const char *)variableName;
@end

externvar id <TkExtra> globalTkInterp;

externvar id <Error>
  WindowCreation,		// error while creating a window
  WindowUsage,                  // error while a window was being used
  PaletteError,
  MissingFiles,
  PixmapError,
  WindowGeometryRecordError;

extern void initTkObjc (id arguments);

