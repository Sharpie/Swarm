function test () {
  var env =
    Components.classes["component://swarm/SwarmEnvironmentImpl"].
      createInstance (Components.interfaces.swarmISwarmEnvironment);

  env.initSwarm_version_bugAddress_argCount_args ("test",
                                                  "0.0",
                                                  "bug-swarm@swarm.org",
                                                  0, []);
  
  var sel =
    Components.classes["component://swarm/SelectorImpl"].
     createInstance (Components.interfaces.swarmISelector);

  var obj =
    Components.classes["component://swarm/objectbase/SwarmObjectImpl"].
     createInstance (Components.interfaces.swarmISwarmObject);

  env.xprint (obj);

  sel.create (obj, "myMethod", false);
}