// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Arguments.h>
#import <SwarmEnvironment.h> // SwarmEnvironment

void
_initSwarm_ (int argc, const char **argv, const char *appName,
      const char *version, const char *bugAddress,
      Class argumentsClass,
      struct argp_option *options,
      int (*optionFunc) (int key, const char *arg),
      BOOL forceBatchMode,
      BOOL inhibitExecutableSearchFlag)
{
  id env = [SwarmEnvironment createBegin];

  [env setArguments:
         [argumentsClass ?: [Arguments_c class]
                         createArgc: argc
                         Argv: argv
                         appName: appName
                         version: version
                         bugAddress: bugAddress
                         options: options
                         optionFunc: optionFunc
                         inhibitExecutableSearchFlag:
                           inhibitExecutableSearchFlag]];
  [env setBatchMode: forceBatchMode];

  [env createEnd];
}

