// Internal function used by the initSwarm* macros to initialize the
// libraries
extern void _initSwarm_ (int argc, const char **argv, const char *appName,
                         const char *version, const char *bugAddress,
                         Class argumentsClass,
                         struct argp_option *options,
                         int (*optionFunc) (int key, const char *arg),
                         BOOL forceBatchMode);

#define STRINGIFY(sym) #sym
#define STRINGIFYSYM(sym) STRINGIFY(sym)
#define APPNAME_STRING STRINGIFYSYM(APPNAME)
#ifdef APPVERSION
#define APPVERSION_STRING STRINGIFYSYM(APPVERSION)
#else
#define APPVERSION_STRING NULL
#endif
#ifdef BUGADDRESS
#define BUGADDRESS_STRING STRINGIFYSYM(BUGADDRESS)
#else
#define BUGADDRESS_STRING NULL
#endif
