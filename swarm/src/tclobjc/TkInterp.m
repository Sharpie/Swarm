/* Implementation for Objective-C Tcl, Tk interpreter object
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
#endif

#include "TkInterp.h"
#if HAVE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif
#include <tk.h>
#include "tclObjc.h"

#define DEFAULT_PROMPT "Tk% "
#define DEFAULT_PARTIAL_PROMPT "Tk> "

// Global variables used by the main program:
static Tk_Window w;		/* The main window for the application.  If
				 * NULL then the application no longer
				 * exists. */

#if HAVE_READLINE
int tk_iter()
{
#if ((TK_MAJOR_VERSION == 4 && TK_MINOR_VERSION >= 1) || TK_MAJOR_VERSION > 4)
  if (Tk_GetNumMainWindows() <= 0)
#else
  if (tk_NumMainWindows <= 0) 
#endif
    { 
      rl_event_hook = 0; 
#if 0
      rl_end_of_line();
      rl_unix_line_discard();
      rl_newline();
      printf("tk_iter aborting\n");
#endif
      return 0; 
    }
  while (Tk_DoOneEvent(TK_ALL_EVENTS | TK_DONT_WAIT));
  return 1;
}
#endif

#if ((TK_MAJOR_VERSION == 4 && TK_MINOR_VERSION == 0) || TK_MAJOR_VERSION < 4)
static int synchronize = 0;
static char *display = NULL;
#endif
static const char *name = NULL;


#if ! HAVE_READLINE
static void	Prompt _ANSI_ARGS_((Tcl_Interp *interp, int partial));
static void	StdinProc _ANSI_ARGS_((ClientData clientData, int mask));
#endif /* ! HAVE_READLINE */

#ifdef __CYGWIN32__
#define Tk_CreateFileHandler(a, b, c, d)
#define Tk_DeleteFileHandler(a)
#endif

@implementation TkInterp

- (const char *)preInitWithArgc: (int)argc argv: (const char **)argv
{
  const char *fileName;
  const char *tclPath = NULL;
  const char *tkPath = NULL;
  
 retry:
  fileName = [super preInitWithArgc:argc argv:argv];

  Tcl_SetVar (interp, "tkObjc", "1", TCL_GLOBAL_ONLY);

  if (tclPath)
    Tcl_SetVar (interp, "tcl_library", (char *)tclPath, TCL_GLOBAL_ONLY);
  
  if (tkPath)
    Tcl_SetVar (interp, "tk_library", (char *)tkPath, TCL_GLOBAL_ONLY);
  
  /*
   * Initialize the Tk application and arrange to map the main window
   * after the startup script has been executed, if any.  This way
   * the script can withdraw the window so it isn't ever mapped
   * at all.
   */
  if (argc)
    name = argv[0];
  else
    name = "tkObjc";

#if ((TK_MAJOR_VERSION == 4 && TK_MINOR_VERSION == 0) || TK_MAJOR_VERSION < 4)
  w = Tk_CreateMainWindow(interp, display, name, "Tk");
  if (w == NULL) {
    fprintf(stderr, "%s\n", interp->result);
    exit(1);
  }
  if (synchronize) {
    XSynchronize(Tk_Display(w), True);
  }
  Tk_GeometryRequest(w, 200, 200);
#endif

  if (Tk_Init (interp) == TCL_ERROR)
    {
      if (tclPath == NULL)
        {
          if ((tclPath = [self checkTclLibrary]) != NULL)
            {
              Tcl_DeleteInterp (interp);
              goto retry;
            }
        }
      else if (tkPath == NULL)
        {
          if (secondaryPath && [self checkPath: secondaryPath file: "tk.tcl"])
            {
              Tcl_DeleteInterp (interp);
              tkPath = secondaryPath;
              goto retry;
            }
        }
      {
        const char *msg = 
          Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
        
        if (msg == NULL)
          msg = interp->result;
        [self error:msg];
        return NULL;		/* shouldn't get here anyway */
      }
    }
#if (TK_MAJOR_VERSION > 4 || (TK_MAJOR_VERSION == 4 && TK_MINOR_VERSION >= 1))
  w = Tk_MainWindow(interp);
  if (w == NULL) {
    fprintf(stderr, "%s\n", interp->result);
    exit(1);
  }
#endif

  return fileName;

}

- initWithArgc: (int)argc argv: (const char **)argv
{
  [super initWithArgc: argc argv: argv];
  stopped = YES;
  //  tclObjc_eventHook = tk_iter;  Think about this;

#if HAVE_READLINE
  rl_event_hook = tk_iter;
#endif

  fflush (stdout);
  {
    char buf[8];

    strcpy (buf, "update");
    (void) Tcl_Eval (interp, buf);
  }

  return self;
}

/* Send this to abort 'promptAndEval' loop */
- stop
{
  stopped = YES;
#if HAVE_READLINE
  rl_beg_of_line();
  rl_kill_line();
  //  rl_stuff_char(EOF);
  rl_stuff_char('\n');
#endif
  return self;
}

- promptAndEval
#if HAVE_READLINE
{
  const char *cmd;
  const char *line;
  int result;
  int gotPartial = 0;
      
  Tcl_DStringInit(&command);
  stopped = NO;
#if (TK_MAJOR_VERSION > 4 || (TK_MAJOR_VERSION == 4 && TK_MINOR_VERSION >= 1))
  while (Tk_GetNumMainWindows() > 0 && !stopped)
#else
  while (tk_NumMainWindows > 0 && !stopped)
#endif
    {
      /* I could add code to do something like tcl_prompt1 */
      if (gotPartial)
	line = readline(DEFAULT_PARTIAL_PROMPT);
      else
	line = readline(DEFAULT_PROMPT);
      if (!line)
	break;
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
    Tk_CreateFileHandler(0, TK_READABLE, StdinProc, (ClientData)self);
    Prompt(interp, 0);
    Tcl_DStringInit(&command);
    stopped = NO;
#if ((TK_MAJOR_VERSION == 4 && TK_MINOR_VERSION >= 1) || TK_MAJOR_VERSION > 4)
    while (Tk_GetNumMainWindows() > 0 && !stopped)
#else
    while (tk_NumMainWindows > 0 && !stopped)
#endif
      Tk_DoOneEvent(0);
    printf("\n");
    return self;
}
#endif

- free
{
  /* Call 'exit' first so that users can redefine 'exit' for their
     own cleanup */
  Tcl_GlobalEval(interp, "exit\n");
  Tcl_GlobalEval(interp, "destroy .\n");
  return [super free];
}

// Allow access to this variable so clever programmers can use Xlib to
// supplement Tk. This Tk_Window is sufficient to get the Display,
// visual, etc.
- (Tk_Window)mainWindow
{
  return w;
}

@end


#if ! HAVE_READLINE

/* The following two functions modified from the Tk distribution: */

/* 
 * main.c --
 *
 *	This file contains the main program for "wish", a windowing
 *	shell based on Tk and Tcl.  It also provides a template that
 *	can be used as the basis for main programs for other Tk
 *	applications.
 *
 * Copyright (c) 1990-1993 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */


/*
 *----------------------------------------------------------------------
 *
 * StdinProc --
 *
 *	This procedure is invoked by the event dispatcher whenever
 *	standard input becomes readable.  It grabs the next line of
 *	input characters, adds them to a command being assembled, and
 *	executes the command if it's complete.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Could be almost arbitrary, depending on the command that's
 *	typed.
 *
 *----------------------------------------------------------------------
 */

    /* ARGSUSED */
static void
StdinProc(clientData, mask)
    ClientData clientData;
    int mask;				/* Not used. */
{
#define BUFFER_SIZE 4000
    char input[BUFFER_SIZE+1];
    static int gotPartial = 0;
    const char *cmd;
    int code, count;

    count = read(fileno(stdin), input, BUFFER_SIZE);
    if (count <= 0) {
	if (!gotPartial) {
	  /***
	    if (tty) {
		Tcl_Eval(interp, "exit");
		exit(1);
	    } else {
		Tk_DeleteFileHandler(0);
	    }
	    ***/
	    [(id)clientData stop];
	    Tk_DeleteFileHandler(0);
	    return;
	} else {
	    count = 0;
	}
    }
    cmd = Tcl_DStringAppend(&(((TkInterp*)clientData)->command), input, count);
    if (count != 0) {
	if ((input[count-1] != '\n') && (input[count-1] != ';')) {
	    gotPartial = 1;
	    goto prompt;
	}
	if (!Tcl_CommandComplete ((char *)cmd)) {
	    gotPartial = 1;
	    goto prompt;
	}
    }
    gotPartial = 0;

    /*
     * Disable the stdin file handler while evaluating the command;
     * otherwise if the command re-enters the event loop we might
     * process commands from stdin before the current command is
     * finished.  Among other things, this will trash the text of the
     * command being evaluated.
     */

    Tk_CreateFileHandler(0, 0, StdinProc, (ClientData)clientData);
    code = Tcl_RecordAndEval(((TclInterp*)clientData)->interp, (char *)cmd, 0);
    Tk_CreateFileHandler(0, TK_READABLE, StdinProc, (ClientData)clientData);
    Tcl_DStringFree(&(((TkInterp*)clientData)->command));
    if (*(((TkInterp*)clientData)->interp)->result != 0) {
        /* clean this up later
	if (1 && (code != TCL_OK) || (tty)) */
	    printf("%s\n", (((TclInterp*)clientData)->interp)->result);
    }

    /*
     * Output a prompt.
     */

    prompt:
    /* clean this up later 
    if (tty) */
	Prompt(((TclInterp*)clientData)->interp, gotPartial);

}

/*
 *----------------------------------------------------------------------
 *
 * Prompt --
 *
 *	Issue a prompt on standard output, or invoke a script
 *	to issue the prompt.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	A prompt gets output, and a Tcl script may be evaluated
 *	in interp.
 *
 *----------------------------------------------------------------------
 */

static void
Prompt(interp, partial)
    Tcl_Interp *interp;			/* Interpreter to use for prompting. */
    int partial;			/* Non-zero means there already
					 * exists a partial command, so use
					 * the secondary prompt. */
{
  /* I could add code to do something like tcl_prompt1 */
  if (partial) 
    fputs(DEFAULT_PARTIAL_PROMPT, stdout);
  else
    fputs(DEFAULT_PROMPT, stdout);
  fflush(stdout);
}

#endif /* ! HAVE_READLINE */
