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

  var list =
    Components.classes["urn:swarm:collections.ListImpl"].
     createInstance (Components.interfaces.swarmICreate);

  list = list.create (env.globalZone);

  list = list.QueryInterface (Components.interfaces.swarmIList);

  sel = sel.create (list, "addLast");

  var obj =
    Components.classes["urn:swarm:objectbase.SwarmObjectImpl"].
     createInstance (Components.interfaces.swarmICreate);
  obj.create (env.globalZone);  

  var fa =
    Components.classes["urn:swarm:defobj.FArgumentsCImpl"].
     createInstance (Components.interfaces.swarmICreateC);

  fa = fa.createBegin (env.globalZone);

  fa = fa.QueryInterface (Components.interfaces.swarmIFArgumentsC);
  fa.setSelector (sel);       
  fa.addObject (obj);
  fa = fa.QueryInterface (Components.interfaces.swarmICreateC);
  fa = fa.createEnd ();

  var fc =
    Components.classes["urn:swarm:defobj.FCallImpl"].
     createInstance (Components.interfaces.swarmIFCall);
                
  fc.create_target_selector_arguments (env.globalZone, list, sel, fa);
  
  fc.performCall ();
  env.xfprint (list);
}
