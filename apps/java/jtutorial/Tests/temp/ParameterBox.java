// ParameterBox.java

import java.awt.*;
import java.awt.event.*;
import java.lang.reflect.Field;
import java.util.*;

public class ParameterBox extends Frame implements WindowListener
{
    boolean firstDisplay = true;
    Button saveButton = new Button("Save");
    Button cancelButton = new Button("Cancel");
    Vector varList;
    Class varClass;

    public ParameterBox(String title, Class vClass)
    {
	super(title);

	varClass = vClass;
	varList = new Vector();

	this.addWindowListener(this);
	saveButton.addActionListener(new saveButtonL());
	cancelButton.addActionListener(new cancelButtonL());
	setResizable(false);
	setBackground(Color.lightGray);
    }

    public ParameterBox(String title, String svClass)
    {
	super(title);

	try
	    {
	    varClass = Class.forName(svClass);
	    } catch (Exception e)
		{
		System.out.println("ParameterBox: invalid class name " 
				   + svClass);
		return;
		}
	varList = new Vector();

	this.addWindowListener(this);
	saveButton.addActionListener(new saveButtonL());
	cancelButton.addActionListener(new cancelButtonL());
	setResizable(false);
	setBackground(Color.lightGray);
    }


    public boolean addVar(String vName)
    {
	Field f;
	Class vType;

	try
	    {
	    f = varClass.getField(vName);
	    vType = f.getType();
	    if (vType.equals(Character.TYPE))
		varList.add(new CharVar(vName, f));
	    else if (vType.equals(Byte.TYPE))
		varList.add(new ByteVar(vName, f));
	    else if (vType.equals(Short.TYPE))
		varList.add(new ShortVar(vName, f));
	    else if (vType.equals(Integer.TYPE))
		varList.add(new IntVar(vName, f));
	    else if (vType.equals(Long.TYPE))
		varList.add(new LongVar(vName, f));
	    else if (vType.equals(Float.TYPE))
		varList.add(new FloatVar(vName, f));
	    else if (vType.equals(Double.TYPE))
		varList.add(new DoubleVar(vName, f));
	    else if (vType.equals(Boolean.TYPE))
		varList.add(new BooleanVar( vName, f));
	    else
		{
		System.out.println("ParameterBox: invalid data type for field "
				   + vName 
				   + "for class "
				   + varClass.toString());
		return false;
		}
	    } catch (Exception e)
		{
		System.out.println("ParameterBox: invalid field name " 
				   + vName 
				   + "for class "
				   + varClass.toString());
		return false;
		}

	return true;
    }

    public boolean display()
    {
	Variable var;
	String str;

	// If this is the first display, set the layout now that we
	// know the number of rows we'll need.  (The last two
	// arguments are the gaps between rows and colums, in pixels.)
	// Then create and add the labels and text fields, and finally
	// add the buttons.
	if (firstDisplay)
	    {
	    this.setLayout(new GridLayout(varList.size()+1, 2, 10, 10));

	    str = "";
	    Iterator i = varList.iterator();
	    while (i.hasNext())
		{
		var = (Variable)i.next();
		var.textField = new TextField(str, 10);
		var.label = new Label(var.varName, Label.RIGHT);
		this.add(var.label);
		this.add(var.textField);
		}
	    this.add(saveButton);
	    this.add(cancelButton);
	    }

	// Get the current values of each variable in the list.
	if (!updateFromObject())
	    {
	    this.dispose();
	    return false;
	    }

	Iterator i = varList.iterator();
	while (i.hasNext())
	    {
	    var = (Variable)i.next();
	    var.textField.setText(var.inString);
	    }
	this.pack();
	this.show();

	return true;
    }

    public boolean updateFromObject()
    {
	Variable var;

	Iterator i = varList.iterator();
	while (i.hasNext())
	    {
	    if (!((Variable)i.next()).setIn())
		return false;
	    }

	return true;
    }

    public void windowClosing(WindowEvent e) 
	{
	this.dispose();
	}

    public void windowOpened(WindowEvent e) {}
    public void windowClosed(WindowEvent e) {}
    public void windowIconified(WindowEvent e) {}
    public void windowDeiconified(WindowEvent e) {}
    public void windowActivated(WindowEvent e) {}
    public void windowDeactivated(WindowEvent e) {}


    public class saveButtonL implements ActionListener
    {
	public void actionPerformed(ActionEvent e)
	{
	    System.out.println("Save button pressed.");
	    //retrieveValues();
	}
    }

    public class cancelButtonL implements ActionListener
    {
	public void actionPerformed(ActionEvent e)
	{
	    System.out.println("Cancel button pressed.");
	}
    }

    private class Variable
    {
	Field varField;
	String varName;

	TextField textField;
	Label label;

	String inString, outString;


	Variable(String vName, Field vField)
	{
	    varName = vName;
	    varField = vField;
	}

	boolean setIn ()
	{
	    return false;
	}
    }

    private class CharVar extends Variable
    {
	char inVal, outVal;

	CharVar(String vName, Field vField)
	{
	    super(vName, vField);
	}

	boolean setIn()
	{
	    try
		{
		inVal = varField.getChar(varClass);
		inString = "" + inVal;
		} catch (Exception e)
		    {
		    System.out.println("ParameterBox: can't convert "
				       + varName);
		    return false;
		    }
	    return true;
	}

	char setOut(char val)
	{
	    outVal = val;
	    return val;
	}

	char getIn()
	{
	    return inVal;
	}

	char getOut()
	{
	    return outVal;
	}
    }

    private class ByteVar extends Variable
    {
	byte inVal, outVal;

	ByteVar(String vName, Field vField)
	{
	    super(vName, vField);
	}

	boolean setIn(byte val)
	{
	    try
		{
		inVal = varField.getByte(varClass);
		inString = Byte.toString(inVal);
		} catch (Exception e)
		    {
		    System.out.println("ParameterBox: can't convert "
				       + varName);
		    return false;
		    }
	    return true;
	}

	byte setOut(byte val)
	{
	    outVal = val;
	    return val;
	}

	byte getIn()
	{
	    return inVal;
	}

	byte getOut()
	{
	    return outVal;
	}
    }
    private class ShortVar extends Variable
    {
	short inVal, outVal;

	ShortVar(String vName, Field vField)
	{
	    super(vName, vField);
	}

	boolean setIn(short val)
	{
		    try
			{
			inVal = varField.getShort(varClass);
			inString = Short.toString(inVal);
			} catch (Exception e)
			    {
			    System.out.println("ParameterBox: can't convert "
					       + varName);
			    return false;
			    }
	    return true;
	}

	short setOut(short val)
	{
	    outVal = val;
	    return val;
	}

	short getIn()
	{
	    return inVal;
	}

	short getOut()
	{
	    return outVal;
	}
    }
    private class IntVar extends Variable
    {
	int inVal, outVal;

	IntVar(String vName, Field vField)
	{
	    super(vName, vField);
	}

	boolean setIn(int val)
	{
	    try
		{
		inVal = varField.getInt(varClass);
		inString = Integer.toString(inVal);
		} catch (Exception e)
		    {
		    System.out.println("ParameterBox: can't convert "
				       + varName);
		    return false;
		    }
	    return true;
	}

	int setOut(int val)
	{
	    outVal = val;
	    return val;
	}

	int getIn()
	{
	    return inVal;
	}

	int getOut()
	{
	    return outVal;
	}
    }
    private class LongVar extends Variable
    {
	long inVal, outVal;

	LongVar(String vName, Field vField)
	{
	    super(vName, vField);
	}

	boolean setIn(long val)
	{
	    try
		{
		inVal = varField.getLong(varClass);
		inString = Long.toString(inVal);
		} catch (Exception e)
		    {
		    System.out.println("ParameterBox: can't convert "
				       + varName);
		    return false;
		    }
	    return true;
	}

	long setOut(long val)
	{	    outVal = val;
	    return val;
	}

	long getIn()
	{
	    return inVal;
	}

	long getOut()
	{
	    return outVal;
	}
    }
    private class FloatVar extends Variable
    {
	float inVal, outVal;

	FloatVar(String vName, Field vField)
	{
	    super(vName, vField);
	}

	boolean setIn(float val)
	{
	    try
		{
		inVal = varField.getFloat(varClass);
		inString = Float.toString(inVal);
		} catch (Exception e)
		    {
		    System.out.println("ParameterBox: can't convert "
				       + varName);
		    return false;
		    }
	    return true;
	}

	float setOut(float val)
	{
	    outVal = val;
	    return val;
	}

	float getIn()
	{
	    return inVal;
	}

	float getOut()
	{
	    return outVal;
	}
    }
    private class DoubleVar extends Variable
    {
	double inVal, outVal;

	DoubleVar(String vName, Field vField)
	{
	    super(vName, vField);
	}

	boolean setIn(double val)
	{
	    try
		{
		inVal = varField.getDouble(varClass);
		inString = Double.toString(inVal);
		} catch (Exception e)
		    {
		    System.out.println("ParameterBox: can't convert "
				       + varName);
		    return false;
		    }
	    return true;
	}

	double setOut(double val)
	{
	    outVal = val;
	    return val;
	}

	double getIn()
	{
	    return inVal;
	}

	double getOut()
	{
	    return outVal;
	}
    }
    private class BooleanVar extends Variable
    {
	boolean inVal, outVal;

	BooleanVar(String vName, Field vField)
	{
	    super(vName, vField);
	}

	boolean setIn(boolean val)
	{
	    try
		{
		inVal = varField.getBoolean(varClass);
		inString = (new Boolean(inVal)).toString();
		} catch (Exception e)
		    {
		    System.out.println("ParameterBox: can't convert "
				       + varName);
		    return false;
		    }
	    return true;
	}

	boolean setOut(boolean val)
	{
	    outVal = val;
	    return val;
	}

	boolean getIn()
	{
	    return inVal;
	}

	boolean getOut()
	{
	    return outVal;
	}
    }


}
