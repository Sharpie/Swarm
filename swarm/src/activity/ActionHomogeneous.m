PHASE(Creating)
+ createBegin: aZone
{
  ACTION_HOMOGENEOUS_TYPE *obj = [super createBegin: aZone];

  obj->targetCount = 0;
#ifdef UPDATEJAVACALL
  obj->javaTargets = NULL;
#endif
  obj->objcTargets = NULL;
  return obj;
}

- (void)setTarget: theTarget
{
  target = theTarget;
}

- createEnd
{
  BOOL allSameFlag = [target allSameClass];

  [super createEnd];
  
  if (allSameFlag)
    {
      id <Index> index = [target begin: getZone (self)];
      
      targetCount = [target getCount];
#ifdef UPDATEJAVATARGET
      if (SD_JAVA_FIND_OBJECT_JAVA ([target getFirst]))
        {
          size_t i;
          id obj;
          
          javaTargets =
            [scratchZone alloc: sizeof (jobject) * targetCount];
          for (i = 0, obj = [index next];
               [index getLoc] == Member;
               obj = [index next], i++)
            javaTargets[i] = SD_JAVA_FIND_OBJECT_JAVA (obj);
        }
      else
#endif
        {
          size_t i;
          id obj;
          
          objcTargets =
            [scratchZone alloc: sizeof (id) * targetCount];
          for (i = 0, obj = [index next];
               [index getLoc] == Member;
               obj = [index next], i++)
            objcTargets[i] = obj;
#ifdef SETUPCALL
          SETUPCALL;
#endif
        }
      
    }
  else
    raiseEvent (InvalidArgument, "Collection not homogeneous");
  return self;
}

PHASE(Setting)
#ifdef SETUPCALL
- (void)setMessageSelector: (SEL)aSel
{
  selector = aSel;

  if (imp)
    SETUPCALL;
}
#endif

- setDefaultOrder: (id <Symbol>)aSymbol
{
  setDefaultOrder (&bits, aSymbol);
  return self;
}


PHASE(Using)

- (void)_performAction_: (id <Activity>)anActivity
{
#ifdef UPDATEJAVATARGET
  if (javaTargets)
    {
      if (getDefaultOrder (bits) == Randomized)
        {
          size_t j = targetCount, k;

          while (j > 1)
            {
              jobject kobj;

              j--;
              k = [uniformUnsRand getUnsignedWithMin: 0 withMax: j];
              kobj = javaTargets[k];
              javaTargets[k] = javaTargets[j];
              javaTargets[j] = kobj;
            }
        }
      {
        size_t i;
        
        for (i = 0; i < targetCount; i++)
          {
            UPDATEJAVATARGET (javaTargets[i]);
            PERFORMJAVACALL (javaTargets[i]);
          }
      }
    }
  else
#endif
  if (objcTargets)
    {
      if (getDefaultOrder (bits) == Randomized)
        {
          size_t j = targetCount, k;

          while (j > 1)
            {
              id kobj;

              j--;
              k = [uniformUnsRand getUnsignedWithMin: 0 withMax: j];
              kobj = objcTargets[k];
              objcTargets[k] = objcTargets[j];
              objcTargets[j] = kobj;
            }
        }
      {
        size_t i;
        
        for (i = 0; i < targetCount; i++)
          {
            UPDATEOBJCTARGET (objcTargets[i]);
            PERFORMOBJCCALL (objcTargets[i]);
          }
      }
    }
  else
    abort ();
}

- (id <Symbol>)getDefaultOrder
{
  return getDefaultOrder (bits);
}

- getTarget
{
  return target;
}

#ifdef SETUPCALL
- (SEL)getMessageSelector
{
  return selector;
}
#endif

- (void)describe: stream
{
  char buffer[100];

  [stream catC: "[[foreach fixed: "];
  _obj_formatIDString (buffer, target);
  [stream catC: buffer];
  [stream catC: "]"];
}

- (void)drop
{
#ifdef UPDATEJAVATARGET
  if (javaTargets)
    [scratchZone free: (void *) javaTargets];
  else
#endif
   if (objcTargets)
    [scratchZone free: (void *) objcTargets];
  [super drop];
}
