// TestWindow.java
import java.awt.*;
import java.io.*;

import jUtils.jGUI.*;

public class TestWindow
{
    public static void main(String[] args)
    {
	TestClass c = new TestClass();

	JavaProbe f = new JavaProbe("TestClass Parameters");

	f.addProbedVariable("testChar", c);
	f.addProbedVariable("testInt", c);
	f.addProbedVariable("testDouble", c);
	f.addProbedVariable("testBoolean", c);

	f.display();

	new JavaStartBox("StartBox");

	System.out.println("Waiting for a keypress.");

	//BufferedReader console = 
	//new BufferedReader(new InputStreamReader(System.in));
	//try
	//{
	//console.readLine();
	//} catch (IOException e) {;}

	System.out.println(c.testChar);
	System.out.println(c.testInt);
	System.out.println(c.testDouble);
	System.out.println(c.testBoolean);

	f.dispose();

	System.exit(0);
    }
}
