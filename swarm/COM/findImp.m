#import <simtools.h> // initSwarmBatch
#import <objc/objc.h>
#import <objc/objc-api.h>

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
                      
                      strcpy (buf, name);
                      for (j = 0; j < len; j++)
                        if (buf[j] == ':')
                          buf[j] = '_';
                      buf[len] = '\0';
                      printf ("_%c_%s__%s\n",
                              factoryFlag ? 'c' : 'i',
                              class->name,
                              buf);
                      found = YES;
                      goto next;
                    }
                }
              methods = methods->method_next;
            }
        }
    next:
      if (!found)
        abort ();
    }
  return 0;
}
/* 
Local Variables: 
compile-command: "$SWARMHOME/bin/libtool-swarm --mode=link gcc -D_GNU_SOURCE -DAPPNAME=findImp -o findImp -Wall -Werror -g -Wno-import -I$SWARMHOME/include -I$SWARMHOME/include/swarm -L$SWARMHOME/lib/swarm findImp.m -lswarm -lobjc" 
End: 
*/
