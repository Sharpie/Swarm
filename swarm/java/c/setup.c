#include "directory.h"
#include <misc.h> // abort

static void initialize (void) __attribute__ ((constructor));
static void shutdown (void) __attribute__ ((destructor));

static void
initialize (void)
{
  java_directory_init ();
}

static void
shutdown (void)
{
  java_directory_drop ();
}
