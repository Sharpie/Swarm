/*
Name:         Holder.h
Description:  Functionality for storing objects, used through mix-in 
              inheritance
Test suite:   activity
*/ 



/* Mix-in inheritance of a Holder class that holds a list of objects.
   Used for for subclassing from activity library classes (ActionGroup, 
   ConcurrentGroup, Schedule etc.).
*/

+ createBegin: (id) aZone numberOfObjects: (int) num;
- addObject: (id) obj;
- createEnd;
- getObjectAt: (int) offset;


