function test () {
  var env =
    Components.classes["urn:swarm:SwarmEnvironmentImpl"].
      createInstance (Components.interfaces.swarmISwarmEnvironment);

  env.initSwarm_version_bugAddress_argCount_args ("test",
                                                  "0.0",
                                                  "bug-swarm@swarm.org",
                                                  0, []);
  
  var sel =
    Components.classes["urn:swarm:SelectorImpl"].
     createInstance (Components.interfaces.swarmISelector);

  var obj =
    Components.classes["urn:swarm:objectbase.SwarmObjectImpl"].
     createInstance (Components.interfaces.swarmISwarmObject);

  var args =
    Components.classes["urn:swarm:defobj.FArgumentsCImpl"].
     createInstance (Components.interfaces.swarmICreateC);

  sel.create (obj, "respondsTo");

  args.createBegin (env.globalZone);

  args = args.QueryInterface (Components.interfaces.swarmIFArgumentsC);
  // args.setSelector (sel);       
  // args.addObject (sel);

  args = args.QueryInterface (Components.interfaces.swarmICreateC);
  args.createEnd ();

  env.xprint (args);
}
