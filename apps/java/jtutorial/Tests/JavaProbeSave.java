// JavaProbe.java

import java.awt.*;
import java.awt.event.*;
import java.lang.reflect.Field;
import java.util.*;

public class JavaProbe extends Frame implements WindowListener
{
    boolean firstDisplay = true;
    Button saveButton = new Button("Save");
    Button cancelButton = new Button("Cancel");
    Vector varList;
    Class varClass;

    public JavaProbe(String title, Object vObject)
    {
	this(title, vObject.getClass());
    }

    public JavaProbe(String title, Class vClass)
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

    public JavaProbe(String title, String svClass)
    {
	super(title);

	try
	    {
	    varClass = Class.forName(svClass);
	    } catch (Exception e)
		{
		System.out.println("JavaProbe: invalid class name " 
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


    public boolean addVar(String vName, Object vObject)
    {
	Field f;
	Class vType;

	try
	    {
	    f = varClass.getField(vName);
	    vType = f.getType();
	    if (vType.equals(Character.TYPE))
		varList.add(new CharVar(vName, f, vObject));
	    else if (vType.equals(Byte.TYPE))
		varList.add(new ByteVar(vName, f, vObject));
	    else if (vType.equals(Short.TYPE))
		varList.add(new ShortVar(vName, f, vObject));
	    else if (vType.equals(Integer.TYPE))
		varList.add(new IntVar(vName, f, vObject));
	    else if (vType.equals(Long.TYPE))
		varList.add(new LongVar(vName, f, vObject));
	    else if (vType.equals(Float.TYPE))
		varList.add(new FloatVar(vName, f, vObject));
	    else if (vType.equals(Double.TYPE))
		varList.add(new DoubleVar(vName, f, vObject));
	    else if (vType.equals(Boolean.TYPE))
		varList.add(new BooleanVar( vName, f, vObject));
	    else
		{
		System.out.println("JavaProbe: invalid data type for field "
				   + vName 
				   + "for class "
				   + varClass.toString());
		return false;
		}
	    } catch (Exception e)
		{
		System.out.println("JavaProbe: invalid field name " 
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
	    firstDisplay = false;
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

    private boolean updateFromObject()
    {
	Variable var;

	Iterator i = varList.iterator();
	while (i.hasNext())
	    {
	    if (!((Variable)i.next()).getFieldValue())
		return false;
	    }

	return true;
    }

    private void saveValues()
    {
	Variable var;
	boolean noErrors = true;

	// Get the string from each text boxe.  If the string has
	// changed, parse the string and, if the parse is successful,
	// set the corresponding outVal and the newVal flag.
	// Otherwise just skip over the textBox.

	Iterator i = varList.iterator();
	while (i.hasNext())
	    {
	    var = (Variable)i.next();
	    var.getNewString();
	    var.newFlag = false;
	    if (!var.outString.equals(var.inString))
		{
		if (var.getNewValue())
		    var.newFlag = true;
		else
		    {
		    noErrors = false;
		    break;
		    }
		}
	    }

	i = varList.iterator();
	while (i.hasNext())
	    {
	    var = (Variable)i.next();
	    if (var.newFlag)
		var.setFieldValue();
	    }
    }

    private void formatError(String vName, String vClass)
    {
	new JavaErrorBox(this, "JavaProbe", "Invalid format for "
			 + vName + ".\nReenter in \""
			 + vClass + "\" format.");
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
	    saveValues();
	}
    }

    public class cancelButtonL implements ActionListener
    {
	public void actionPerformed(ActionEvent e)
	{
	    display();
	}
    }

    private class Variable
    {
	Field varField;
	String varName;
	Object varObject, outVal;
	TextField textField;
	Label label;
	String inString, outString;
	boolean newFlag = false;

	Variable(String vName, Field vField, Object vObject)
	{
	    varName = vName;
	    varField = vField;
	    varObject = vObject;
	}

	boolean getFieldValue ()
	{
	    System.out.println("We should never get here.");
	    return false;
	}

	boolean setFieldValue()
	{
	    try
		{
		varField.set(varObject, outVal);
		}catch (Exception e)
		    {
		    System.out.println("JavaProbe: unable to set new "
				       + "value for " + varName);
		    return false;
		    }
	    return true;
	}

	void getNewString ()
	{
	    outString = textField.getText();
	}

	boolean getNewValue ()
	{
	    System.out.println("We should never get here.");
	    return false;
	}

    }

    private class CharVar extends Variable
    {
	char inVal;

	CharVar(String vName, Field vField, Object vObject)
	{
	    super(vName, vField, vObject);
	}

	boolean getFieldValue()
	{
	    try
		{
		inVal = varField.getChar(varObject);
		inString = "" + inVal;
		} catch (Exception e)
		    {
		    System.out.println("JavaProbe: can't convert "
				       + varName);
		    return false;
		    }
	    return true;
	}

	boolean getNewValue()
	{
	    try
		{
		outVal = new Character(outString.charAt(0));
		} catch (Exception e)
		    {
		    formatError(varName, varField.getType().getName());
		    return false;
		    }
	    return true;
	}
    }

    private class ByteVar extends Variable
    {
	byte inVal;

	ByteVar(String vName, Field vField, Object vObject)
	{
	    super(vName, vField, vObject);
	}

	boolean getFieldValue()
	{
	    try
		{
		inVal = varField.getByte(varObject);
		inString = Byte.toString(inVal);
		} catch (Exception e)
		    {
		    System.out.println("JavaProbe: can't convert "
				       + varName);
		    return false;
		    }
	    return true;
	}

	boolean getNewValue()
	{
	    try
		{
		outVal = new Byte(outString);
		} catch (Exception e)
		    {
		    formatError(varName, varField.getType().getName());
		    return false;
		    }
	    return true;
	}
    }
    private class ShortVar extends Variable
    {
	short inVal;

	ShortVar(String vName, Field vField, Object vObject)
	{
	    super(vName, vField, vObject);
	}

	boolean getFieldValue()
	{
		    try
			{
			inVal = varField.getShort(varObject);
			inString = Short.toString(inVal);
			} catch (Exception e)
			    {
			    System.out.println("JavaProbe: can't convert "
					       + varName);
			    return false;
			    }
	    return true;
	}

	boolean getNewValue()
	{
	    try
		{
		outVal = new Short(outString);
		} catch (Exception e)
		    {
		    formatError(varName, varField.getType().getName());
		    return false;
		    }
	    return true;
	}
    }
    private class IntVar extends Variable
    {
	int inVal;

	IntVar(String vName, Field vField, Object vObject)
	{
	    super(vName, vField, vObject);
	}

	boolean getFieldValue()
	{
	    try
		{
		inVal = varField.getInt(varObject);
		inString = Integer.toString(inVal);
		} catch (Exception e)
		    {
		    System.out.println("JavaProbe: can't convert "
				       + varName);
		    return false;
		    }
	    return true;
	}

	boolean getNewValue()
	{
	    try
		{
		outVal = new Integer(outString);
		} catch (Exception e)
		    {
		    formatError(varName, varField.getType().getName());
		    return false;
		    }
	    return true;
	}
    }
    private class LongVar extends Variable
    {
	long inVal;

	LongVar(String vName, Field vField, Object vObject)
	{
	    super(vName, vField, vObject);
	}

	boolean getFieldValue()
	{
	    try
		{
		inVal = varField.getLong(varObject);
		inString = Long.toString(inVal);
		} catch (Exception e)
		    {
		    System.out.println("JavaProbe: can't convert "
				       + varName);
		    return false;
		    }
	    return true;
	}

	boolean getNewValue()
	{
	    try
		{
		outVal = new Long(outString);
		} catch (Exception e)
		    {
		    formatError(varName, varField.getType().getName());
		    return false;
		    }
	    return true;
	}
    }
    private class FloatVar extends Variable
    {
	float inVal;

	FloatVar(String vName, Field vField, Object vObject)
	{
	    super(vName, vField, vObject);
	}

	boolean getFieldValue()
	{
	    try
		{
		inVal = varField.getFloat(varObject);
		inString = Float.toString(inVal);
		} catch (Exception e)
		    {
		    System.out.println("JavaProbe: can't convert "
				       + varName);
		    return false;
		    }
	    return true;
	}

	boolean getNewValue()
	{
	    try
		{
		outVal = new Float(outString);
		} catch (Exception e)
		    {
		    formatError(varName, varField.getType().getName());
		    return false;
		    }
	    return true;
	}
    }

    private class DoubleVar extends Variable
    {
	double inVal;

	DoubleVar(String vName, Field vField, Object vObject)
	{
	    super(vName, vField, vObject);
	}

	boolean getFieldValue()
	{
	    try
		{
		inVal = varField.getDouble(varObject);
		inString = Double.toString(inVal);
		} catch (Exception e)
		    {
		    System.out.println("JavaProbe: can't convert "
				       + varName);
		    return false;
		    }
	    return true;
	}

	boolean getNewValue()
	{
	    try
		{
		outVal = new Double(outString);
		} catch (Exception e)
		    {
		    formatError(varName, varField.getType().getName());
		    return false;
		    }
	    return true;
	}
    }

    private class BooleanVar extends Variable
    {
	boolean inVal;

	BooleanVar(String vName, Field vField, Object vObject)
	{
	    super(vName, vField, vObject);
	}

	boolean getFieldValue()
	{
	    try
		{
		inVal = varField.getBoolean(varObject);
		inString = (new Boolean(inVal)).toString();
		} catch (Exception e)
		    {
		    System.out.println("JavaProbe: can't convert "
				       + varName);
		    return false;
		    }
	    return true;
	}

	boolean getNewValue()
	{
	    outVal = new Boolean(outString);
	    return true;
	}
    }
}
