// JavaProbe.java

// JavaProbe is a simple probe into the instance variables of an
// object.  It may be used to set the value of the probed variables,
// or simply to display them.  More than one object can be probed in
// the same window.

// To use JavaProbe, first create an instance of a JavaProbe object,
// passing to it the title for the probe window:

// JavaProbe p = new JavaProbe("title");

// Then add the variables to be probed using the 

// p.addProbedVariable(String "variableName", Object object)

// method where variableName is the name of the variable to be probed
// and object is a pointer to the object in which an instance of it is
// contained.  Once all the desired variables have been added, the
// p.display() method is used to display the probe window.  When the
// window is displayed, the user can view and change the value of any
// variable.  Pressing the SAVE button saves the new values back into
// the probed object.  Pressing the CANCEL button cancels any changes
// made in the probe window since the last SAVE.  Closing the window
// acts like CANCEL.

// JavaProbe uses only the standard awt library.

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

    // The constructor creates a new Frame for the probe window and a
    // Vector to hold the list of variables to be probed.  It then
    // sets up the actionListeners for the SAVE and CANCEL buttons,
    // and sets a few charateristics of the probe window.
    public JavaProbe(String title)
    {
	super(title);

	varList = new Vector();

	this.addWindowListener(this);
	saveButton.addActionListener(new saveButtonL());
	cancelButton.addActionListener(new cancelButtonL());
	setResizable(false);
	setBackground(Color.lightGray);
    }

    // This method adds a variable to the list of variables to be
    // probed.  It does this by determining what type of variable is
    // being added to the list (Character, Integer, Double, etc.) and
    // creating the appropriate object to hold the necessary
    // information about the variable and the object that contains it.
    // If an error occurs, the variable is not added to the list and
    // addProbedVariable returns a boolean false.  The acceptable
    // variable types are clear from the if/else if structure below.
    public boolean addProbedVariable(String vName, Object vObject)
    {
	Field f;
	Class vType;

	try
	    {
	    f = vObject.getClass().getField(vName);
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
				   + vObject.getClass().toString());
		return false;
		}
	    } catch (Exception e)
		{
		System.out.println("JavaProbe: invalid field name " 
				   + vName 
				   + "for class "
				   + vObject.getClass().toString());
		return false;
		}

	return true;
    }

    // The display() method displays the probe window and the current
    // values of the probed variables.
    public boolean display()
    {
	Variable var;
	String str;

	// If this is first invocation of display(), set the layout
	// now that we know the number of rows we'll need.  (The last
	// two arguments are the gaps between rows and colums, in
	// pixels.)  Then create and add the labels and text fields,
	// and finally add the buttons.
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

	// Get the current value of each variable in the list.  If
	// updateFromObject returns an error (false), bail out.
	if (!updateFromObject())
	    {
	    this.dispose();
	    return false;
	    }

	// Now put the current value of each variable in its
	// textField, and force the display of the window.
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

    // This internal method gets the current value of each probed
    // variable from the object that contains it.  It uses the
    // getFieldValue() method defined for each variable type.  (See
    // the Variable class and its subclasses below.)
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

    // This method saves any values that the use has changed.
    private void saveValues()
    {
	Variable var;
	boolean noErrors = true;

	// Get the string from each textField using the getNewString()
	// method defined for each variable type.  If the string has
	// changed, use the getNewValue() method defined for each
	// variable type to parse the string and, if the parse is
	// successful, set the newVal flag.  Otherwise, if an error
	// occurs in parsing a string, break out of the method without
	// saving anything.
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
		    return;
		}
	    }

	// If no errors have occured in parsing the changed variables,
	// save the values of those variables that have change back in
	// the objects that contain them.
	i = varList.iterator();
	while (i.hasNext())
	    {
	    var = (Variable)i.next();
	    if (var.newFlag)
		var.setFieldValue();
	    }
    }

    // This method is called by the string parsing methods to report
    // an error to the user.
    private void formatError(String vName, String vClass)
    {
	new JavaErrorBox(this, "JavaProbe", "Invalid format for "
			 + vName + ".\nReenter in \""
			 + vClass + "\" format.");
    }

    // These methods are required by the WindowListener to respond to
    // the indicated events.  We need to handle only the closing of
    // the probe window.
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

    // The next two classes contain the methods which are called by
    // the ActionListener when either of the buttons is pressed.
    public class saveButtonL implements ActionListener
    {
	public void actionPerformed(ActionEvent e)
	{
	    saveValues();
	}
    }

    // Pressing CANCEL redisplays the probe window.  display() calls
    // updateFromObject() which gets the current values of each
    // variable from the containing object, erasing any changes the
    // user might have made since the last successful save.
    public class cancelButtonL implements ActionListener
    {
	public void actionPerformed(ActionEvent e)
	{
	    display();
	}
    }

    // This is a superclass for the classes defined for each variable
    // type.  It hold some information common to each variable type,
    // and several methods that are overridden by the class for each
    // variable type.
    private class Variable
    {
	Object varObject;
	Field varField;
	String varName;
	Object outVal;
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

	// This method is overridden by the class for each variable
	// type.  It gets the current value of the probed variable
	// from the containing object.
	boolean getFieldValue ()
	{
	    System.out.println("We should never get here.");
	    return false;
	}

	// This method saves the new value of a variable back into the
	// containing object.
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

	// This method gets the current string in a variable's textField.
	void getNewString ()
	{
	    outString = textField.getText();
	}

	// This method is overridden for each variable type.  It
	// parses the outString and creates an appropriate outVal
	// object to hold the result.
	boolean getNewValue ()
	{
	    System.out.println("We should never get here.");
	    return false;
	}

    }

    // The following classes are specific to each to the types of
    // variables that can be probed.  Each defines the type-specific
    // methods that are used to get the values of a probed variable
    // from an object, and that are used to parse a string to extract
    // a new value.
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
