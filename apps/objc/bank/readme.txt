
Contents of this directory:

  1. A new release of << GraphLib-0.2 >>.

  2. A new release of BankNet which has been updated to conform to the 
     latest <random.h> API in Swarm 1.0.1 (as well as the changes in 
     GraphLib itself). 

  3. A new demo (DynamicGraph) which generates and drops nodes dynamically 
     so as to address the questions raised by Alex Lancaster. 

  **********************************************************************
  * Note that in order to drop nodes dynamically the 'NodeItem.m' file *
  * needs to be dropped into '...wherever.../swarm/src/tkobjc/' and    *
  * recompiled (by typing make in that directory).                     *
  **********************************************************************

  4. I have also folded some code into the DiGraph class which was written 
     by Thor Sigvaldason and which basically attempts a simple form of graph 
     layout based on spring embedding (stolen from a well known Java applet 
     distributed by Sun). The code has to translate from a 'list-of-lists'
     representation to a matrix based representation on every basic layout
     step (and has quite a few hard-coded constants), but it is a great
     start and can be worked on by the user-community (it is fast enough
     to do on-the-fly on a Pentium Linux-box).

     The related methods are:

       -setSpringLength: (float) aLength ;
       -boingDistribute: (int) iterations ;
       -boingDistribute ;
       -(double) boingStep ;

     Some of these methods are demonstrated in the DynamicGraph demo...

   5. Two screenshots are included which show the result of using the
      different layout algorithms.
