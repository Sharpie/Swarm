// Java Heatbugs application. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

// All added comments copyright 2001 Timothy Howe. All rights reserved. 

import swarm.Globals;
import swarm.objectbase.Swarm;
import swarm.objectbase.SwarmImpl;
import swarm.simtoolsgui.GUISwarm;
import swarm.simtoolsgui.GUISwarmImpl;

/**
See HeatbugModelSwarm for an overview of the heatbugs application.

<p>
Properies recognized by this program:

 <dir>
 c=&lt;boolean&gt;: Start the Heatbugs all in a contiguous cluster
 </dir>

 <dir>
 d=&lt;double&gt;:  Specify the diffusion rate (0..1)
 </dir>

 <dir>
 e=&lt;double&gt;:  Specify the "evaporation" (really retention) rate (0..1)
 </dir>

 <dir>
 i=&lt;boolean&gt;: Make Heatbugs immobile
 </dir>

 <dir>
 n=&lt;integer&gt;: Specify the number of heatbugs
 </dir>

 <dir>
 p=&lt;int&gt;:     Specify level of diagnostic messages (try 10 or 100)
 </dir>

 <dir>
 r=&lt;double&gt;:  Specify the random-move probability
 </dir>

<p>
In batch mode, the Java properties mechanism herein 
takes precedence over all other setting of variables, 
overriding the SCM file, which 
itself overrides the Java initializers and constructors. 

<p>
In GUI mode, the probe display 
takes precedence over all other setting of variables, 
overriding the Java properties mechanism herein, which 
itself overrides the Java initializers and constructors. 

<p>
See current.ksh for a way to convert command-line options into Java properties.

<p>
To modify this application to accept an 
additional boolean command-line option:

 <dir>
 Add to this documentation section a line analogous to the line
  <dir>
  <xmp>
    c=&lt;boolean&gt;: Start the Heatbugs all in a contiguous cluster
  </xmp>
  </dir>
 </dir>

 <dir>
 and add to the code below a statement analogous to the line
  <dir>
  <xmp>
    model.setStartInOneCluster (getBooleanProperty ("c", false));
  </xmp>
  </dir>
 </dir>

<p>
To modify this application to accept an 
additional integer command-line option:

 <dir>
 Add to this documentation section a line analogous to the line
  <dir>
  <xmp>
    n=&lt;integer&gt;: Specify the number of heatbugs
  </xmp>
  </dir>
 </dir>

 <dir>
 If you do not want this file to apply a default
 overriding the model's default, 
 add to the code below a statement analogous to the statement
  <dir>
  <xmp>
    model.setRandomMoveProbability 
     (getDoubleProperty ("r", model.getRandomMoveProbability ()));
  </xmp>
  </dir>

 If you do want this file to apply a default
 overriding the model's default,
 add a statement analogous to 

  <dir>
  <xmp>
    model.setRandomMoveProbability 
     (getDoubleProperty ("r", 0.5));
  </xmp>
  </dir>
 </dir>

<p>
The documentation in the Swarm Reference Guide for the Arguments protocol
describes a different mechanism for managing 
command-line options (with an example in Objective-C). 

*/
public class StartHeatbugs
{

public static void main (String[] args)
{
    // Swarm initialization: all Swarm apps must call this first:
    Globals.env.initSwarm
     ("jheatbugs", "2.1", "bug-swarm@swarm.org", args);
    HeatbugModelSwarm model;

    if (Globals.env.guiFlag)
    {
        // We want graphics, so make an ObserverSwarm to get GUI objects:
        HeatbugObserverSwarm topLevelSwarm
         = new HeatbugObserverSwarm (Globals.env.globalZone);
        Globals.env.setWindowGeometryRecordName (topLevelSwarm, "topLevelSwarm");
        model = topLevelSwarm.getHeatbugModelSwarm ();
        build (topLevelSwarm, model);
        topLevelSwarm.go ();
        unbuild (topLevelSwarm, model);
    }
    else
    {
        // We do not want graphics, so make a BatchSwarm for writing to files: 
        HeatbugBatchSwarm topLevelSwarm
         = (HeatbugBatchSwarm) Globals.env.lispAppArchiver.getWithZone$key
         (Globals.env.globalZone, "batchSwarm");
        model = topLevelSwarm.getHeatbugModelSwarm ();
        build (topLevelSwarm, model);
        topLevelSwarm.go ();
        unbuild (topLevelSwarm, model);
    }
} /// main()

private static void build (Swarm swarm, HeatbugModelSwarm model)
{
    // ... We'd like to move as much code as possible from the if/then sections
    // in main() into this common method build(), but if we move 
    // getHeatbugModelSwarm(), we get this compile error: *** Error: No method 
    // named "getHeatbugModelSwarm" was found in type "swarm/objectbase/Swarm". 
    // Similarly when we try to move go(). So we moved the calls we could
    // move and left getHeatbugModelSwarm() and go() behind. 

    // We could pass immobile to the batch buildObjects(), but we get a compile
    // error if we try to pass it to the GUI buildObjects(). So instead we use 
    // setImmobile() for both:
    model.setImmobile (getBooleanProperty ("i", false));

    model.setPrintDiagnostics (getIntProperty ("p", 0));

    model.setStartInOneCluster (getBooleanProperty ("c", false));

    model.setRandomMoveProbability 
     (getDoubleProperty ("r", model.getRandomMoveProbability ()));
      // ... In cases such as this, when we happen not to want to override any 
      // default set by HeatbugModelSwarm, we apply that value as our own 
      // default. 

    model.setDiffusionConstant 
     (getDoubleProperty ("d", model.getDiffusionConstant ()));

    model.setEvaporationRate 
     (getDoubleProperty ("e", model.getEvaporationRate ()));

    model.setNumBugs (getIntProperty ("n", -1));
    // ... The special value -1 tells HeatbugModelSwarm to use its own default
    // value for numBugs. Use of Integer rather than int would avoid the need
    // for this special value. 

    swarm.buildObjects ();
    swarm.buildActions ();
    swarm.activateIn (null);
}

private static void unbuild (Swarm swarm, HeatbugModelSwarm model)
{
    swarm.drop ();
}

/**
This method and the other get...Property() methods are generic methods
that would normally be defined in some utility library.
*/
private static boolean getBooleanProperty (String propertyName, boolean dflt)
{
    String property = System.getProperty (propertyName);
    if (property == null || property.equals ("")) return dflt;
    else return property.equals ("true") || property.equals ("1");
}
private static double getDoubleProperty (String propertyName, double dflt)
{
    String property = System.getProperty (propertyName);
    if (property == null || property.equals ("")) return dflt;
    else return Double.parseDouble (property);
}
private static int getIntProperty (String propertyName, int dflt)
{
    String property = System.getProperty (propertyName);
    if (property == null || property.equals ("")) return dflt;
    else return Integer.parseInt (property);
}
private static String getStringProperty (String propertyName, String dflt)
{
    String property = System.getProperty (propertyName);
    if (property == null) return dflt;
    return property;
}

}
