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
#import <defobj/swarm-objc-api.h>

#define EXIT abort ()

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
  int argn; 

  initSwarmBatch (1, argv);
  swarm_force_references ();

  impClass = swarm_objc_lookupClass (argv[1]);
  if (!impClass)
    EXIT;

  for (argn = 2; argn <  argc; argn++)
    {
      Class class;
      SEL sel;
      BOOL factoryFlag;
      BOOL found = NO;
      
      sel = swarm_sel_getUid (&argv[argn][1]);
      if (!sel)
        EXIT;
    
      if (swarm_class_getName(impClass)) 
        imp = swarm_class_getMethodImplementation (impClass, sel);
      else
        imp = NULL;
      if (!imp)
        EXIT;
      
      factoryFlag = argv[argn][0] == '+';

      for (class = factoryFlag ? swarm_class_getMetaclass(impClass) : impClass; 
           class;
           class = swarm_class_getSuperclass(class))
        {
#if SWARM_OBJC_DONE
          struct objc_method_list *methods = class->methods;
          
          while (methods)
            {
              int i;
              
              for (i = 0; i < methods->method_count; i++)
                {
                  if (swarm_sel_isEqual (methods->method_list[i].method_name, sel))
                    {
                      const char *name = swarm_sel_getName (sel);
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
#else
	  unsigned i, outCount;
	  ObjcMethod *methodList = swarm_class_copyMethodList(class, &outCount);

	  if (methodList) {
	    for (i = 0; i < outCount; i++) {
	      if (swarm_sel_isEqual (swarm_method_getName(methodList[i]), sel)) {
		const char *name = swarm_sel_getName (sel);
		//printf("%s\n", name);
		size_t len = strlen (name);
		char buf[len + 1];
		unsigned j;
		const char *className = strip (swarm_class_getName(class));
                      
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
	    free(methodList);
	  }
#endif
        }
    next:
      if (!found)
        fprintf (stderr, "Could not find `%s' in `%s'\n",
                 swarm_sel_getName (sel), strip (swarm_class_getName(impClass)));
          
    }
  return 0;
}
/* 
Local Variables: 
compile-command: "$SWARMHOME/bin/libtool-swarm --mode=link gcc -D_GNU_SOURCE -DAPPNAME=findImp -o findImp -Wall -Werror -g -Wno-import -I$SWARMHOME/include -I$SWARMHOME/include/swarm -L$SWARMHOME/lib/swarm findImp.m -lswarm -lobjc" 
End: 
*/
