#import <awtobjc/JavaObject.h>

typedef enum {
  EVTMOUSE,
  EVTWIN
} EvtType;

// possible events we can get from Java
typedef enum {
  EVT_MOUSE_CLICKED = 0,
  EVT_MOUSE_MOVED,
  EVT_MOUSE_DRAGGED,
  EVT_MOUSE_ENTERED,
  EVT_MOUSE_EXITED,
  EVT_MOUSE_PRESSED,
  EVT_MOUSE_RELEASED,
  EVT_MOUSE_LAST
} MouseEvt;
#define EVT_MOUSE_FIRST	EVT_MOUSE_CLICKED


// possible events we can get from Java
typedef enum {
  EVT_WIN_RESIZE = 0,
  EVT_WIN_CLOSE,
  EVT_WIN_LAST
} WinEvt;

typedef struct evtchain {
  id obj;
  SEL msg;
  struct evtchain * next;
} EvtChain;

typedef struct {
  char * evtName;
  int evtCode;
  EvtChain * chain;
} EvtMap;


@interface JavaInput: JavaObject
{
  // map from java event code to one we understand
  EvtMap mouseMap[EVT_MOUSE_LAST];
  EvtMap winMap[EVT_WIN_LAST];

  jclass _mouseClass;

  jmethodID _getMouseID;
  jmethodID _getSource;
  jmethodID _getMouseX;
  jmethodID _getMouseY;

  id _winClass;

  jmethodID _getEvent;
}

- (jobject)getEvent;
- processMessage: (jobject)event;
- bindMessage: (SEL)message EventType: (EvtType)type 
        Event: (int)event On: (id)obj;
- checkEvents;

extern id inputQueue;

@end

