/*
Name:         Holder.m
Description:  Functionality for storing objects, used through mix-in 
              inheritance
Library:      activity
*/ 



/* Mix-in inheritance of a Holder class that holds a list of objects.
   Used for for subclassing from activity library classes (ActionGroup, 
   ConcurrentGroup, Schedule etc.).
*/

#ifdef CLASS_NAME

#ifdef MIXIN_CREATE
+ createBegin: aZone numberOfObjects: (int)num
{
  CLASS_NAME *obj = [super createBegin: aZone];
  obj->numberOfObjects = num;
  obj->objects = [aZone alloc: num * sizeof (id)];
  obj->counter = 0;
  return obj;
}

- addObject: obj
{
  if (counter < numberOfObjects)
    *(objects + counter++) = obj;
  return self;
}

#else
- getObjectAt: (int)offset
{
  if (offset < counter)
    return *(objects + offset);
  else
    return NULL;
}
#endif
#endif
