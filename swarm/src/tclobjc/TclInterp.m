/* Implementation for Objective-C Tcl interpreter object
   Copyright (C) 1993,1994  R. Andrew McCallum

   Written by:  R. Andrew McCallum <mccallum@cs.rochester.edu>
   Dept. of Computer Science, U. of Rochester, Rochester, NY  14627

   This file is part of the Tcl/Objective-C interface library.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/ 

#ifdef NeXT
#include "objc-gnu2next.h"
#include <objc/List.h>
#include <objc/HashTable.h>
#else
#include "List.h"
#include "HashTable.h"
#endif /* NeXT */

#include "TclInterp.h"
#include "tclObjc.h"

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h> // malloc
#include <unistd.h> // access

#if HAVE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif /* HAVE_READLINE */

#define DEFAULT_PROMPT "Tcl% "
#define DEFAULT_PARTIAL_PROMPT "Tcl> "

/* If we're using tcl 7.4 or greater, must make tcl_RcFileName reference
 * be extern or else we get a linker conflict. In tcl 7.3, you must
 * declare it here or else it links in the wrong file.
 *
 * And the variable was changed for tcl 7.5 from a C variable to a 
 * Tcl variable, so it doesn't have to be declared here anymore.  
 * These defines were moved to TclInterp.h. 
 */
#if (TCLVERSIONLT74 == 0)
#   if (TCLVERSIONGT74 == 0)
extern char* tcl_RcFileName;
#   endif
#else
char *tcl_RcFileName = NULL;	/* Name of a user-specific startup script
				 * to source if the application is being run
				 * interactively (e.g. "~/.wishrc").  Set
				 * by Tcl_AppInit.  NULL means don't source
				 * anything ever. */
#endif
List* tclList;

@implementation TclInterp

+ initialize
{
  if (self == [TclInterp class])
    {
      tclList = [[List alloc] init];
    }
  return self;
}

+ firstTcl
{
  if ([tclList count])
    return [tclList objectAt:0];
  else
    {
      fprintf(stderr, "no firstTcl\n");
      return nil;
    }
}

+ tclAtIndex: (unsigned) index
{
  if (index < [tclList count])
    return [tclList objectAt:index];
  return nil;
}

+ (unsigned) tclCount
{
  return [tclList count];
}

- setSecondaryLibraryPath: (const char *)path
{
  secondaryPath = path;
  return self;
}

- (BOOL)checkPath: (const char *)path file: (const char *)file
{
  char buf[strlen (path) + strlen (file) + 2];
  
  strcpy (buf, path);
  strcat (buf, "/");
  strcat (buf, file);

  return access (buf, R_OK) != -1;
}

- (const char *)checkTclLibrary
{
  const char *originalPath =
    Tcl_GetVar (interp, "tcl_library", TCL_GLOBAL_ONLY);

  if (![self checkPath: originalPath file: "init.tcl"])
    {
      if (secondaryPath && [self checkPath: secondaryPath file: "init.tcl"])
        {
          Tcl_SetVar (interp,
                      "tcl_library",
                      (char *)secondaryPath,
                      TCL_GLOBAL_ONLY);
          return secondaryPath;
        }
      else
        return NULL;
    }
  else
    {
      char *buf = malloc (strlen (originalPath) + 1);

      if (buf == NULL)
        abort ();
      strcpy (buf, originalPath);
      return buf;
    }
}

- (const char *)preInitWithArgc: (int)argc argv: (const char **)argv
{
  const char *args;
  const char *fileName;

  /* tell class object about us */
  [tclList addObject: self];

  /* Create objc name and id hashtables */
  namesToObjects = [[HashTable alloc] initKeyDesc:"*"
		    valueDesc:"@"];
  objectsToNames = [[HashTable alloc] initKeyDesc:"@"
		    valueDesc:"*"];

  /* Create and init tcl interpreter */

  interp = Tcl_CreateInterp ();

  [self checkTclLibrary];
  
  /*
   * Make command-line arguments available in the Tcl variables "argc"
   * and "argv".
   */
  fileName = NULL;
  if ((argc > 1) && (argv[1][0] != '-'))
    {
      fileName = argv[1];
      argc--;
      argv++;
    }

  args = Tcl_Merge (argc-1, (char **)argv+1);
  Tcl_SetVar (interp, "argv", (char *)args, TCL_GLOBAL_ONLY);
  ckfree ((void *)args);
  {
    char buffer[20];
    
    sprintf (buffer, "%d", argc-1);
    Tcl_SetVar (interp, "argc", buffer, TCL_GLOBAL_ONLY);
  }
  Tcl_SetVar (interp, "argv0",
              (char *)((fileName != NULL) ? fileName : argv[0]),
              TCL_GLOBAL_ONLY);
  
  Tcl_SetVar (interp, "tcl_interactive", "1", TCL_GLOBAL_ONLY);
  Tcl_SetVar (interp, "tclObjc", tclObjc_objectToName(self), TCL_GLOBAL_ONLY);
  
  if (Tcl_Init (interp) == TCL_ERROR || TclObjc_Init(interp) == TCL_ERROR)
    {
      char *msg = Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
      if (msg == NULL)
	msg = interp->result;
      [self error:msg];
      return NULL;		/* shouldn't get here anyway */
    }
  /* Specify a user-specific startup file to invoke if the application
     is run interactively.  Typically the startup file is "~/.apprc"
     where "app" is the name of the application.  If this line is
     deleted then no user-specific startup file will be run under any
     conditions. */

#if (TCLVERSIONGT74 == 1)
  Tcl_SetVar(interp, "tcl_rcFileName", "~/.wishrc", TCL_GLOBAL_ONLY);
#else
  tcl_RcFileName = "~/.wishrc";
#endif

  return fileName;
}

- initWithArgc: (int)argc argv: (const char **)argv
{
  const char *fileName, *msg;

  [super init];

#if HAVE_READLINE
  if (argc)
    rl_readline_name = argv[0];
#endif

  fileName = [self preInitWithArgc:argc argv:argv];
  tclObjc_registerObjectWithName(interp, self, "objcTcl");

  /* If a script file was specified then source that file. */
  if (fileName) 
    if (Tcl_EvalFile(interp, (char *)fileName) != TCL_OK) 
      goto error;

  return self;

 error:
  msg = Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
  if (msg == NULL) {
    msg = interp->result;
  }
  [self error:msg];
  [self free];
  return nil;
}

- init
{
  return [self initWithArgc:0 argv:NULL];
}

- free
{
  [tclList removeObject:self];
  Tcl_DeleteInterp(interp);
  [namesToObjects free];
  [objectsToNames free];
  return [super free];
}

- setEvalDebugPrint: (BOOL) value
{
  evalDebugPrint = value;
  return self;
}

- eval: (const char *)fmt, ...
{
  char cmd[32768];   /* Ugly constant.  Get rid of this */
  va_list ap;

  va_start(ap,fmt);
  vsprintf(cmd, fmt, ap);

  if (evalDebugPrint)
    fprintf(stderr, "%s\n", cmd);

  code = Tcl_Eval(interp, cmd);
  if (code != TCL_OK)
    {
      char *msg;
      msg = Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
      if (msg == NULL) {
	msg = interp->result;
      }
      fprintf(stderr, "(Tcl -eval:) %s\n", msg);
      fprintf(stderr, "while evaluating: %s\n", cmd);
    }
  va_end(ap);
  return self;
}

- globalEval: (const char *)fmt, ...
{
  char cmd[4096];
  va_list ap;

  va_start(ap,fmt);
  vsprintf(cmd, fmt, ap);

  if (evalDebugPrint)
    fprintf(stderr, "(global) %s\n", cmd);

  code = Tcl_GlobalEval(interp, cmd);
  if (code != TCL_OK)
    {
      char *msg;
      msg = Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
      if (msg == NULL) {
	msg = interp->result;
      }
      fprintf(stderr, "(Tcl -eval:) %s\n", msg);
      fprintf(stderr, "while evaluating: %s\n", cmd);
    }
  va_end(ap);
  return self;
}

- evalFile: (const char *)filename
{
  if ((code = Tcl_EvalFile(interp, (char*)filename)) != TCL_OK) 
    {
      char *msg;
      msg = Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
      if (msg == NULL) {
	msg = interp->result;
      }
      fprintf(stderr, "(Tcl -evalFile:) %s\n", msg);
      fprintf(stderr, "while evaluating contents of file %s\n", filename);
    }
  return self;
}

- (BOOL) variableExists: (const char *)varName
{
  return (Tcl_GetVar(interp, (char*)varName, 0) != NULL);
}

- (BOOL) globalVariableExists: (const char *)varName
{
  return (Tcl_GetVar(interp, (char*)varName, TCL_GLOBAL_ONLY) != NULL);
}

- (const char *) variableValue: (const char *)varName
{
  const char *v = Tcl_GetVar(interp, (char*)varName, 0);
  if (!v)
    fprintf(stderr, "(Tcl variableValue:) %s isn't a variable\n", varName);
  return v;
}

- (const char *) globalVariableValue: (const char *)varName
{
  const char *v = Tcl_GetVar(interp, (char*)varName, TCL_GLOBAL_ONLY);
  if (!v)
    fprintf(stderr, "(Tcl variableValue:) %s isn't a variable\n", varName);
  return v;
}

- (int) code
{
  return code;
}

- (const char *) result
{
  return interp->result;
}

- (Tcl_Interp *) interp
{
  return interp;
}

/* I should make all these rely on the intepretter's hash tables instead.
   That way we can get names defined by Tcl "set" commands too. */

- registerObject: (id)anObject withName: (const char *)aName
{
  [namesToObjects insertKey:aName value:anObject];
  [objectsToNames insertKey:anObject value: (char *)aName];
  tclObjc_registerObjectWithName (interp, anObject, aName);
  return self;
}

- unregisterObject: (id)anObject
{
  char *name = (char *)[objectsToNames valueForKey:anObject];

  tclObjc_unregisterObjectNamed(interp, name);
  [objectsToNames removeKey:anObject];
  [namesToObjects removeKey:name];
  return self;
}

- unregisterObjectNamed:(const char *)aName
{
  return [self unregisterObject:
	       (id)[namesToObjects valueForKey:(char *)aName]];
}

- (const char *) nameForObject:anObject
{
  return (char *)[objectsToNames valueForKey:anObject];
}
    
- objectNamed:(const char *)aName
{
  id theObject = nil;

  if ((theObject = tclObjc_nameToObject(aName)) != (id)-1)
    return theObject;
  else if ((theObject = [namesToObjects valueForKey:aName]))
    return theObject;
  return (id)-1;
}

- (BOOL) objectIsRegistered: anObject
{
  return [objectsToNames isKey:anObject];
}

- (BOOL) nameIsRegistered: (const char *)aName
{
  return [namesToObjects isKey:aName];
}

- promptAndEval
#if HAVE_READLINE
{
  Tcl_DString command;
  const char *cmd;
  const char *line;
  int result;
  int gotPartial = 0;
      
  Tcl_DStringInit(&command);
  while (1)
    {
      /* I could add code to do something like tcl_prompt1 */
      if (gotPartial)
	line = readline(DEFAULT_PARTIAL_PROMPT);
      else
	line = readline(DEFAULT_PROMPT);
      if (!line)
	{
	  printf("\n");
	  return self;
	}
      add_history(line);
      cmd = Tcl_DStringAppend(&command, line, -1);
      free(line);
      if (!Tcl_CommandComplete(cmd))
	{
	  gotPartial = 1;
	  continue;
	}
      gotPartial = 0;
      result = Tcl_RecordAndEval(interp, cmd, 0);
      Tcl_DStringFree(&command);
      if (result != TCL_OK)
	fprintf(stderr, "%s\n", interp->result);
      else
	printf("%s\n", interp->result);
    }
  return self;
}
#else /* HAVE_READLINE */
{
    char buffer[1000];
    const char *cmd;
    int result, gotPartial;
    static Tcl_DString command;	/* Used to buffer incomplete commands being
				 * read from stdin. */

    gotPartial = 0;
    Tcl_DStringInit(&command);
    for (;;) {
	clearerr(stdin);
	/* I could add code to do something like tcl_prompt1 */
	if (gotPartial)
	  fputs (DEFAULT_PARTIAL_PROMPT, stdout);
	else
	  fputs (DEFAULT_PROMPT, stdout);
	fflush (stdout);
	if (fgets (buffer, 1000, stdin) == NULL) {
	    if (!gotPartial) {
	      printf("\n");
	      return self;
	    }
	    buffer[0] = 0;
	}
	cmd = Tcl_DStringAppend(&command, buffer, -1);
	if ((buffer[0] != 0) && !Tcl_CommandComplete ((char *)cmd)) {
	    gotPartial = 1;
	    continue;
	}

	gotPartial = 0;
	result = Tcl_RecordAndEval (interp, (char *)cmd, 0);
	Tcl_DStringFree(&command);
	if (result != TCL_OK) {
	    fprintf(stderr, "%s\n", interp->result);
	} else if (*interp->result != 0) {
	    printf("%s\n", interp->result);
	}
    }
    return self;
}
#endif /* HAVE_READLINE */

@end

