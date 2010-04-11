#include <misc.h>
#include <sys/timeb.h>

#if 0
int
gettimeofday (struct timeval *tv, struct timezone *tz)
{
   struct _timeb tb;

   if (!tv)
      return (-1);

  _ftime (&tb);
  tv->tv_sec  = tb.time;
  tv->tv_usec = tb.millitm * 1000 + 500;
  
  if (tz) 
    {
      tz->tz_minuteswest = -60 * _timezone;
      tz->tz_dsttime = _daylight;
    }
  return (0);
}
#endif
