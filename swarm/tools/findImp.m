// Copyright © 2000 Swarm Development Group
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <simtools.h> // initSwarmBatch
#import <objc/objc.h>
#import <objc/objc-api.h>

static const char *
strip (const char *className)
{
  char *buf = SSTRDUP (className);
  char *ptr = strstr (buf, ".Creating");

  if (ptr)
    *ptr = '\0';
  return buf;
}

int
main (int argc, const char **argv)
{
  Class impClass;
  IMP imp;
  unsigned argn;

  initSwarmBatch (1, argv);

  impClass = objc_lookup_class (argv[1]);
  if (!impClass)
    abort ();

  for (argn = 2; argn < argc; argn++)
    {
      Class class;
      SEL sel;
      BOOL factoryFlag;
      BOOL found = NO;
      
      sel = sel_get_any_uid (&argv[argn][1]);
      if (!sel)
        abort ();
      
      imp = get_imp (impClass, sel);
      if (!imp)
        abort ();
      
      factoryFlag = argv[argn][0] == '+';
      
      for (class = factoryFlag ? impClass->class_pointer : impClass; 
           class;
           class = class->super_class)
        {
          struct objc_method_list *methods = class->methods;
          
          while (methods)
            {
              unsigned i;
              
              for (i = 0; i < methods->method_count; i++)
                {
                  if (sel_eq (methods->method_list[i].method_name, sel))
                    {
                      const char *name = sel_get_name (sel);
                      size_t len = strlen (name);
                      char buf[len + 1];
                      unsigned j;
                      const char *className = strip (class->name);
                      
                      strcpy (buf, name);
                      for (j = 0; j < len; j++)
                        if (buf[j] == ':')
                          buf[j] = '_';
                      buf[len] = '\0';
                      printf ("%c%s _%c_%s__%s\n",
                              factoryFlag ? '+' : '-',
                              name,
                              factoryFlag ? 'c' : 'i',
                              className,
                              buf);
                      SFREEBLOCK (className);
                      found = YES;
                      goto next;
                    }
                }
              methods = methods->method_next;
            }
        }
    next:
      if (!found)
        fprintf (stderr, "Could not find `%s' in `%s'\n",
                 sel_get_name (sel), strip (impClass->name));
          
    }
  return 0;
}
/* 
Local Variables: 
compile-command: "$SWARMHOME/bin/libtool-swarm --mode=link gcc -D_GNU_SOURCE -DAPPNAME=findImp -o findImp -Wall -Werror -g -Wno-import -I$SWARMHOME/include -I$SWARMHOME/include/swarm -L$SWARMHOME/lib/swarm findImp.m -lswarm -lobjc" 
End: 
*/
