// ParameterBox.java

import java.awt.*;
import java.awt.event.*;
import java.lang.reflect.Field;
import java.util.*;

public class ParameterBox extends Frame implements WindowListener
{
    int nRows = 1;
    boolean firstDisplay = true;
    Button saveButton = new Button("Save");
    Button cancelButton = new Button("Cancel");
    Vector varList;
    Class varClass;

    char CHAR = 'C', BYTE = 'B', SHORT = 'S', INT = 'I';
    char LONG = 'L', FLOAT = 'F', DOUBLE = 'D', BOOLEAN = 'O';

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


    public boolean addVar(String sField)
    {
	Field f;
	Class vType;
	char vTypeChar = 0;

	try
	    {
	    f = varClass.getField(sField);
	    vType = f.getType();
	    if (vType.equals(Character.TYPE))
		vTypeChar = CHAR;
	    else if (vType.equals(Byte.TYPE))
		vTypeChar = BYTE;
	    else if (vType.equals(Short.TYPE))
		vTypeChar = SHORT;
	    else if (vType.equals(Integer.TYPE))
		vTypeChar = INT;
	    else if (vType.equals(Long.TYPE))
		vTypeChar = LONG;
	    else if (vType.equals(Float.TYPE))
		vTypeChar = FLOAT;
	    else if (vType.equals(Double.TYPE))
		vTypeChar = DOUBLE;
	    else if (vType.equals(Boolean.TYPE))
		vTypeChar = BOOLEAN;
	    else
		{
		System.out.println("ParameterBox: invalid data type for field "
				   + sField 
				   + "for class "
				   + varClass.toString());
		return false;
		}

	    varList.add(new Variable(sField, f, vTypeChar));
	    ++nRows;

	    } catch (Exception e)
		{
		System.out.println("ParameterBox: invalid field name " 
				   + sField 
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
	    this.setLayout(new GridLayout(nRows, 2, 10, 10));

	    str = "";
	    Iterator i = varList.iterator();
	    while (i.hasNext())
		{
		var = (Variable)i.next();
		var.textField = new TextField(str, 10);
		var.label = new Label(var.varLabel, Label.RIGHT);
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
	    switch ((var = (Variable)i.next()).varType)
		{
		case 'C':
		    str = "" + var.valueChar;
		    break;

		case 'B':
		    str = Byte.toString(var.valueByte);
		    break;

		case 'S':
		    str = Short.toString(var.valueShort);
		    break;

		case 'I':
		    str = Integer.toString(var.valueInt);
		    break;

		case 'L':
		    str = Long.toString(var.valueLong);
		    break;

		case 'F':
		    str = Float.toString(var.valueFloat);
		    break;

		case 'D':
		    str = Double.toString(var.valueDouble);
		    break;

		case 'O':
		    str = (new Boolean(var.valueBoolean)).toString();
		    break;

		default:
		    str = "";
		}

	    var.textField.setText(str);
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
	    switch ((var = (Variable)i.next()).varType)
		{
		case 'C':
		    try
			{
			var.valueChar = var.varField.getChar(varClass);
			} catch (Exception e)
			    {
			    System.out.println("ParameterBox: can't convert "
					       + var.varLabel);
			    return false;
			    }
		    break;

		case 'B':
		    try
			{
			var.valueByte = var.varField.getByte(varClass);
			} catch (Exception e)
			    {
			    System.out.println("ParameterBox: can't convert "
					       + var.varLabel);
			    return false;
			    }
		    break;

		case 'S':
		    try
			{
			var.valueShort = var.varField.getShort(varClass);
			} catch (Exception e)
			    {
			    System.out.println("ParameterBox: can't convert "
					       + var.varLabel);
			    return false;
			    }
		    break;

		case 'I':
		    try
			{
			var.valueInt = var.varField.getInt(varClass);
			} catch (Exception e)
			    {
			    System.out.println("ParameterBox: can't convert "
					       + var.varLabel);
			    return false;
			    }
		    break;

		case 'L':
		    try
			{
			var.valueLong = var.varField.getLong(varClass);
			} catch (Exception e)
			    {
			    System.out.println("ParameterBox: can't convert "
					       + var.varLabel);
			    return false;
			    }
		    break;

		case 'F':
		    try
			{
			var.valueFloat = var.varField.getFloat(varClass);
			} catch (Exception e)
			    {
			    System.out.println("ParameterBox: can't convert "
					       + var.varLabel);
			    return false;
			    }
		    break;

		case 'D':
		    try
			{
			var.valueDouble = var.varField.getDouble(varClass);
			} catch (Exception e)
			    {
			    System.out.println("ParameterBox: can't convert "
					       + var.varLabel);
			    return false;
			    }
		    break;

		case 'O':
		    try
			{
			var.valueBoolean = var.varField.getBoolean(varClass);
			} catch (Exception e)
			    {
			    System.out.println("ParameterBox: can't convert "
					       + var.varLabel);
			    return false;
			    }
		    break;
		    }
	    }

	return true;
    }

    public boolean retrieveValues()
    {
	Variable var;
	String str;

	char c;
	byte b;
	short s;
	long l;
	int j;
	float f;
	double d;
	boolean o;

	Iterator i = varList.iterator();
	while (i.hasNext())
	    {
	    var = (Variable)i.next();
	    str = var.textField.getText();
	    switch (var.varType)
		{
		case 'C':
		    try
			{
			c = str.charAt(0);
			} catch (Exception e)
			    {
			    System.out.println("ParameterBox: can't convert "
					       + var.varLabel);
			    return false;
			    }
		    break;

		case 'B':
		    try
			{
			b = Byte.parseByte(str);
			} catch (Exception e)
			    {
			    System.out.println("ParameterBox: can't convert "
					       + var.varLabel);
			    return false;
			    }
		    break;

		case 'S':
		    try
			{
			s = Short.parseShort(str);
			} catch (Exception e)
			    {
			    System.out.println("ParameterBox: can't convert "
					       + var.varLabel);
			    return false;
			    }
		    break;

		case 'I':
		    try
			{
			j = Integer.parseInt(str);
			} catch (Exception e)
			    {
			    System.out.println("ParameterBox: can't convert "
					       + var.varLabel);
			    return false;
			    }
		    break;

		case 'L':
		    try
			{
			l = Long.parseLong(str);
			} catch (Exception e)
			    {
			    System.out.println("ParameterBox: can't convert "
					       + var.varLabel);
			    return false;
			    }
		    break;

		case 'F':
		    try
			{
			f = Float.parseFloat(str);;
			} catch (Exception e)
			    {
			    System.out.println("ParameterBox: can't convert "
					       + var.varLabel);
			    return false;
			    }
		    break;

		case 'D':
		    try
			{
			d = Double.parseDouble(str);
			} catch (Exception e)
			    {
			    System.out.println("ParameterBox: can't convert "
					       + var.varLabel);
			    return false;
			    }
		    break;

		case 'O':
		    if (Boolean.getBoolean(str))
			o = true;
		    else
			o = false;
		    break;
		    }
	    }

	return true;
    }

    public void saveValues()
    {
	if (retrieveValues())
	    saveToObject();
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
	    retrieveValues();
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
	String varLabel;
	Field varField;
	char varType;
	TextField textField;
	Label label;

	// declare local variables of the various possible types to
	// hold the value of the probed variable.  We might be more
	// economical by using the Number class, but this is more
	// straightforward.
	char valueChar;
	byte valueByte;
	short valueShort;
	int valueInt;
	long valueLong;
	float valueFloat;
	double valueDouble;
	boolean valueBoolean;

	Variable(String vLabel, Field vField, char vType )
	{
	    varLabel = vLabel;
	    varField = vField;
	    varType = vType;
	}
    }
}
