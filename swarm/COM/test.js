// netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect");
// document.write ("Hello world<BR>");
  var clazz = Components.classes["component://swarm/SwarmEnvironmentImpl"];
  var iface = Components.interfaces.swarmISwarmEnvironment;
  
  var env = clazz.createInstance (iface);
  env.initSwarm_version_bugAddress_argCount_args ("test", "0.0", "bug-swarm@swarm.org", 0, []);
  
  var iface = env.globalZone.QueryInterface (Components.interfaces.swarmIDefinedObject);
  // iface.xprint ();

