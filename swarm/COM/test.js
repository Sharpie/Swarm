function test () {
  var env =
    Components.classes["component://swarm/SwarmEnvironmentImpl"].
      createInstance (Components.interfaces.swarmISwarmEnvironment);

  env.initSwarm_version_bugAddress_argCount_args ("test",
                                                  "0.0",
                                                  "bug-swarm@swarm.org",
                                                  0, []);

  env.xprint (env.globalZone);
  
  var sel =
    Components.classes["component://swarm/SelectorImpl"].
     createInstance (Components.interfaces.swarmISelector);

  sel.create (this, "myMethod", false);
}