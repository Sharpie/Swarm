function testgui () {
  var env =
    Components.classes["component://swarm/SwarmEnvironmentImpl"].
      createInstance (Components.interfaces.swarmISwarmEnvironment);

  env.initSwarm_version_bugAddress_argCount_args ("test",
                                                  "0.0",
                                                  "bug-swarm@swarm.org",
                                                  0, []);

  var guiSwarm = 
    Components.classes["component://swarm/simtoolsgui/GUISwarmImpl"].
      createInstance (Components.interfaces.swarmICreate);

  guiSwarm = guiSwarm.create (env.globalZone);

  var swarm =
    guiSwarm.QueryInterface (Components.interfaces.swarmISwarm);

  swarm.buildObjects ();
  swarm.buildActions ();

  guiSwarm.QueryInterface (Components.interfaces.swarmIActionType).
    activateIn (null);

  guiSwarm.QueryInterface (Components.interfaces.swarmIGUISwarm).
    go ();
}