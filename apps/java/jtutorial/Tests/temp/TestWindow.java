// TestWindow.java
import java.awt.*;
import java.io.*;

public class TestWindow
{
    public static char testChar = 'C';
    public static int testInt = 20;
    public static double testDouble = 12.333;
    public static boolean testBoolean = true;

    public static void main(String[] args)
    {
	//ParameterBox.VarField vField;

	ParameterBox f = new ParameterBox("TestWindow" + "Parameters", 
					  "TestWindow");

	f.addVar("testChar");
	f.addVar("testInt");
	f.addVar("testDouble");
	f.addVar("testBoolean");

	f.display();

	BufferedReader console = 
	    new BufferedReader(new InputStreamReader(System.in));
	try
	    {
	    console.readLine();
	    } catch (IOException e) {;}

	//f.drop();
	System.exit(0);
    }
}
