// TestWindow.java

import java.awt.*;
import java.io.*;

public class TestStart
{
    public static void main(String[] args)
    {
	new JavaStartBox("Starter", "Press START when ready.", "START");

	System.out.println("We're ready to move on.");

	BufferedReader console = 
	    new BufferedReader(new InputStreamReader(System.in));
	try
	    {
	    console.readLine();
	    } catch (IOException e) {;}

	System.exit(0);
    }
}
