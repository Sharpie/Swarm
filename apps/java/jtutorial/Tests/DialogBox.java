import java.awt.*;
import java.awt.event.*;

public class DialogBox extends Dialog implements WindowListener
{
    Button okButton = new Button(" OK ");

    public DialogBox(Frame parent, String title, String message)
    {
	// Create a Dialog with the specified title and parent. The
	// third argument specified whether the Dialog is modal or
	// not.  (With a modal Dialog, everything stops until the
	// Dialog is closed.)
	super(parent, title, true);

	// Register the WindowListener, specify that the Dialog is not
	// resizable, and set its color.
	this.addWindowListener(this);
	setResizable(false);
	setBackground(Color.lightGray);

	//Create and use a BorderLayout manager with 15 pixel spacing.
	this.setLayout(new BorderLayout(15, 15));

	// Create a Label component to hold the message and add it to
	// the center portion of the window.
	this.add("Center", new Label(message));

	// Set the ActionListener on the OK button.  Then create a
	// Panel to hold the OK button, specify its layout, add the
	// button to the panel, and add the panel to the bottom of the
	// Dialog.
	okButton.addActionListener(new okButtonL());
	Panel p = new Panel();
	p.setLayout(new FlowLayout(FlowLayout.CENTER, 15, 15));
	p.add(okButton);
	this.add("South", p);

	// Set the dialog size to the preferred size of its components
	// and show it.
	this.pack();
	this.show();
    }

    // These are the various methods that could conceivably be called
    // by the WindowListener.  The only event that we need to worry
    // about is when the user closes the window.
    public void windowClosing(WindowEvent e) 
	{
	this.hide();
	this.dispose();
	}
    public void windowOpened(WindowEvent e) {}
    public void windowClosed(WindowEvent e) {}
    public void windowIconified(WindowEvent e) {}
    public void windowDeiconified(WindowEvent e) {}
    public void windowActivated(WindowEvent e) {}
    public void windowDeactivated(WindowEvent e) {}

    // Pop down the window when the button is clicked. This is the
    // class we gave the ActionListener for the OK button.
    public class okButtonL implements ActionListener
    {
	public void actionPerformed(ActionEvent e)
	{
	    DialogBox.this.hide();
	    DialogBox.this.dispose();
	}
    }


    public static void main(String[] args)
    {
	Frame f = new Frame("DialogBox Test");
	f.setSize(100,100);
	f.show();

	DialogBox d = new DialogBox(f, "DialogBox", "Here is a very long message xxxxxxxxxxxxxx.");

	System.out.println("We're done.");
	f.dispose();
	System.exit(0);
    }

}
