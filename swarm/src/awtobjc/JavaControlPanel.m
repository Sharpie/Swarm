// Swarm library. Copyright (C) 1996-1998, 2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/JavaControlPanel.h>
#if 0
#import <simtools/ActionCache.h>
#import <simtools/ControlPanel.h>
#endif
#import <simtools.h> // ControlStateRunning..
#import <awtobjc/JavaEnv.h> // javaEnv, findClass:

@implementation JavaControlPanel;

+ createBegin: aZone
{
  JavaControlPanel *obj = [super createBegin: aZone];

  // set up some local state
  obj->_getState = 0;
  obj->_setState = 0;
  obj->_waitRun = 0;

  return obj;
}

void sendAction (JNIEnv *jniEnv, jobject obj, 
                 jlong actionCache, jobject actName,
                 jlong actType, jlong arg)
{
  const char *cmd = (*jniEnv)->GetStringUTFChars (jniEnv, actName, NULL);
  fprintf(stderr, "whooo whee... sendAction called with ");
  fprintf(stderr, "%s %x\n", cmd, (unsigned)actType);

  // make a new action and drop it in to be executed
  [(id)(int)actionCache sendActionOfType: (id)(int)actType toExecute: cmd];
  (*jniEnv)->ReleaseStringUTFChars (jniEnv, actName, cmd);
}

JNINativeMethod methods[] = {
  {"sendAction", "(JLjava/lang/String;JJ)V", (void *)sendAction}
};

- setActionCache: actionCache
{
  jmethodID setActionCache;
  jclass jactionid;
  JNIEnv *_env;

  // First, find the java action cache class and add a native
  // method so that when it sends actions, they go back to objective
  // c, where they will get added to the action cache.
  printf ("get swarm/Action\n");
  jactionid = [javaEnv findClass: "swarm/Action"];
  printf ("(%p) getJNIEnv\n", jactionid);
  _env = [javaEnv getJNIEnv];
  printf ("(%p)\n", _env);

  if ((*_env)->RegisterNatives (_env, jactionid, methods, 1) < 0)
    fprintf (stderr, "ERROR: couldn't register sendAction\n");

  // give the objc pointer to the java object
  setActionCache = [self findMethod: "setActionCache" 
                         signature: "(I)V"];
#if 1
  // no O: argument method, -mgd
  [self callVoidMethod: setActionCache: (int)actionCache];
#endif

#if 0
  // load up some actions that it can invoke
  [self addAction: "Start" : Control];
  [self addAction: "Stop" : Control];
  [self addAction: "Step" : Control];
  [self addAction: "Next" : Control];
  [self addAction: "Quit" : Control];
  [self addAction: "Probe" : Probing];
#endif
  return self;
}

- addAction: (const char *)actName : actId
{
  // give the objc pointer to the java object
  if (_addAction == 0)
    _addAction = [self findMethod: "addAction" 
                       signature: "(Ljava/lang/String;I)V"];
  [self callVoidMethod: _addAction S: (char *)actName I: (int)actId];
  return self;
}

static int
mapToJState (id state)
{
  int jstate = -1;
  if (state == ControlStateRunning)
    jstate = JControlStateRunning;
  else if (state == ControlStateStopped)
    jstate = JControlStateStopped;
  else if (state == ControlStateStepping)
    jstate = JControlStateStepping;
  else if (state == ControlStateNextTime)
    jstate = JControlStateNextTime;
  else if (state == ControlStateQuit)
    jstate = JControlStateQuit;

fprintf(stderr, "mapping from state %x->%d\n", (unsigned)state, jstate);
  return jstate;
}

static id
mapFromJState (int jstate)
{
  id state = nil;

  switch (jstate)
    {
    case JControlStateRunning:
      state = ControlStateRunning;
      break;
    case JControlStateStopped:
      state = ControlStateStopped;
      break;
    case JControlStateStepping:
      state = ControlStateStepping;
      break;
    case JControlStateNextTime:
      state = ControlStateNextTime;
      break;
    case JControlStateQuit:
      state = ControlStateQuit;
      break;
    }
  fprintf(stderr, "mapping from state %d->%x\n", jstate, (unsigned)state);
  return state;
}

- getState
{
  if (_getState == 0)
    _getState = [self findMethod: "getState" signature: "()I"];
  
  return mapFromJState ([self callIntMethod: _getState]);
}

- setState: state
{
  if (_setState == 0)
    _setState = [self findMethod: "setState" signature: "(I)V"];
  
  [self callVoidMethod: _setState : mapToJState(state)];
  return self;
}

- (void)waitRun
{
  if (_waitRun == 0)
    _waitRun = [self findMethod: "waitRUN" signature: "()V"];
  
  [self callVoidMethod: _waitRun];
}

@end

