// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/global.h>
#import <awtobjc/JavaEnv.h>
#import <awtobjc/JavaInput.h>

#import <awtobjc/common.h>

#include <stdlib.h>

// the input queue
id inputQueue;

//  Implementation of a queue of input events.  There is one queue of input
//  events generated from Java (JavaJInput).  There is one instance of this
//  class which maintains mappings
//  from input events to ObjC messages and, during idle periods, drains
//  input events from the Java side and invokes the appropriate messages.

static char * mouseName[EVT_MOUSE_LAST] = {
  "MOUSE_CLICKED",
  "MOUSE_MOVED",
  "MOUSE_DRAGGED",
  "MOUSE_ENTERED",
  "MOUSE_EXITED",
  "MOUSE_PRESSED",
  "MOUSE_RELEASED"
};


@implementation JavaInput;

- createEnd
{
  int i;

  [super createEnd];

  // Change constructor to look at `queue'

  _getEvent = 0;

  // Extract constant values for the various mouse, window, 
  // and keyboard events and construct a mapping.
  _mouseClass = [env findClass: "java/awt/event/MouseEvent"];

  for (i=(int)EVT_MOUSE_FIRST; i<(int)EVT_MOUSE_LAST; i++)
    {
      mouseMap[i].evtCode = getStaticIntField (jniEnv, _mouseClass, mouseName[i]);
      mouseMap[i].evtName = mouseName[i];
    }
  _getMouseID = findMethod (jniEnv, _mouseClass, "getID", "()I");
  _getSource = findMethod (jniEnv, _mouseClass, "getSource", "()Ljava/lang/Object;");
  _getMouseX = findMethod (jniEnv, _mouseClass, "getX", "()I");
  _getMouseY = findMethod (jniEnv, _mouseClass, "getY", "()I");

  return self;
}

- (jobject)getEvent
{

  // return the next event from the Java input queue; 
  // return null if there is none
  if (_getEvent == 0)
    _getEvent = [self findMethod: "getEvent" signature: "()Ljava/awt/AWTEvent;"];

  return [self callObjectMethod: _getEvent];
}



// process an event; if there's a mapping from this event type to
// an Objective C message, send the appropriate message
- processMessage: (jobject)event
{
  const char *eventName = "unknown";

  // what class event is this, really?

  if (1)
    {
      int i;
      
      // mouse events
      
      // make us an object and extract the event code
      int code = (*jniEnv)->CallIntMethod (jniEnv, event, _getMouseID);
      jobject lobj = (*jniEnv)->CallObjectMethod (jniEnv, event, _getSource);
      
      // XXX we should use something better than a linear search
      for (i=(int)EVT_MOUSE_FIRST; i<(int)EVT_MOUSE_LAST; i++) {
        
        // find the type of event we just got
        if (mouseMap[i].evtCode == code)
          {
            EvtChain * ch;
            int x = (*jniEnv)->CallIntMethod (jniEnv, event, _getMouseX);
            int y = (*jniEnv)->CallIntMethod (jniEnv, event, _getMouseY);
            
            eventName = mouseMap[i].evtName;
            fprintf(stderr, "got event %s on obj %x %d,%d\n", eventName,
                    (unsigned)lobj, (int)x, (int)y);
            
            // see if we have a message defined for this object
            for (ch = mouseMap[i].chain; ch != NULL; ch = ch->next)
              {
                jobject ref = [ch->obj getJobject];
                
                // look for a match with the underlying java object
                if ((*jniEnv)->IsSameObject (jniEnv, ref, lobj))
                  {
                    
                    fprintf(stderr, "Processing event %d %s for object %x\n",
                            code, eventName, (unsigned)lobj);
                    
                    [ch->obj perform: ch->msg with: (id)x with: (id)y];
                    
                    (*jniEnv)->DeleteLocalRef (jniEnv, lobj);
                    return self;
                  }
              }
            break;
          }
        
        if (i == EVT_MOUSE_LAST)
          fprintf(stderr, "Unrecognized mouse code %d\n", code);
      }
      
      (*jniEnv)->DeleteLocalRef (jniEnv, lobj);
    }
  
  return self;
}

// establish a mapping between an event within this object and an Objective
// C message
- bindMessage: (SEL)msg EventType: (EvtType)type 
        Event: (int)event On: (id)object
{
  // if we get the given incoming event on the java object underlying
  // the object passed in, invoke the given message on it
  if (type == EVTMOUSE)
    {
      EvtChain * ch = (EvtChain *)malloc(sizeof(EvtChain));
      ch->obj = object;
      ch->msg = msg;
      ch->next = mouseMap[event].chain;
      
      mouseMap[event].chain = ch;
      
      fprintf(stderr, "BIND: event %d\n", event);
      
    }
  else
    {
      fprintf(stderr, "bindMessage: Invalid message type\n");
      exit(1);
    }
  
  return self;
}

// process all outstanding events
- checkEvents
{
  jobject nextEvent;

  while ((nextEvent = [self getEvent]) != NULL)
    [self processMessage: nextEvent];
  
  return self;
}

@end
