/*
 * @(#)AlignmentOriginConstraint.java	1.01 96/04/18 Eric Lunt
 * Copyright (c) 1996 All Rights Reserved.
 *
 * Version history:
 * 1.01 04/18/96
 *      Formalized packaging into com.bdnm.awt.
 *      Clean-up code & comments
 * 1.0  02/28/96
 *      Initial creation of FractionalLayout, OriginConstraint,
 *          AlignmentOriginConstraint, and FrameConstraint.
 */
import java.awt.*;

/**
 * An AlignmentOriginConstraint is used with an instance of
 * FractionalLayout to describe the layout of an unbounded
 * figure.  It provides for the 'location point' of the figure to be
 * inset from the top and left edges of the figure.
 *
 * A couple of examples:
 * <ul>
 * <li><code>new AlignmentOriginConstraint(0.0, 0, 1.0, -10, 1.0, 0.0)</code>
 * will place the upper-right corner of the component 10 pixels to the left
 * of the upper-right corner of the container.  The component
 * will be its preferred size.
 * <li><code>new AlignmentOriginConstraint(0.5, 0, 0.5, 0, 0.5, 0.5)</code>
 * will place the center of the component in the center of the container.
 * The component will be its preferred size.
 * </ul>
 *
 * @see FractionalLayout
 * @version 1.01, 04/18/96
 * @author Eric Lunt (elunt@mcs.net)
 */
public class AlignmentOriginConstraint extends OriginConstraint {

	/**
	 * A number between 0.0 and 1.0 which represents the inset
	 * of the location point from the left edge of the component.
	 */
	public double leftAlignmentFraction;

	/**
	 * A number between 0.0 and 1.0 which represents the inset
	 * of the location point from the top edge of the component.
	 */
	public double topAlignmentFraction;

	/**
	 * Create an instanace of an AlignmentOriginConstraint.  This
	 * will place the upper-left corner of the component at the
	 * upper-left corner of the container.
	 */
	public AlignmentOriginConstraint() {
		super();
	}

	/**
	 * Create an instance of an AlignmentOriginConstraint given the
	 * left fraction, the left offset, the top fraction, the top offset,
	 * the left alignment fraction, and the top alignment fraction.
	 * @param lf	the left fraction
	 * @param l		the left offset
	 * @param tf	the top fraction
	 * @param t		the top offset
	 * @param laf	the left alignment fraction
	 * @param taf	the top alignment fraction
	 */
	public AlignmentOriginConstraint(double lf, int l, double tf, int t,
		double laf, double taf) {
		super(lf,l,tf,t);
		leftAlignmentFraction = laf;
		topAlignmentFraction = taf;
	}

	/**
	 * Given the size of a container and the component size,
	 * return a Rectangle which appropriately bounds the component
	 * given this constraint.
	 * @param containerSize	the size of the container
	 * @param componentSize	the size of the component
	 * @return	the containing rectangle
	 */
	public Rectangle adjustedRectangle(Dimension containerSize,
		Dimension componentSize) {

		Rectangle newRect = super.adjustedRectangle(containerSize,componentSize);
		newRect.translate(-adjustedAlignmentLeft(componentSize),-adjustedAlignmentTop(componentSize));
		return newRect;
	}

	/**
	 * Given a component size, figure out the appropriate horizontal offset
	 * @param   componentSize   the size of the component
	 * @return  the pixel offset value for the left edge of the component
	 */
	protected int adjustedAlignmentLeft(Dimension componentSize) {
		return (int) Math.round(leftAlignmentFraction*componentSize.width);
	}

	/**
	 * Given a component size, figure out the appropriate vertical offset
	 * @param   componentSize   the size of the component
	 * @return  the pixel offset value for the top edge of the component
	 */
	protected int adjustedAlignmentTop(Dimension componentSize) {
		return (int) Math.round(topAlignmentFraction*componentSize.height);
	}

	/**
	 * Given the size of a component, return the smallest size
	 * container which could contain it given this constraint.
	 * @param componentSize the size of a component
	 * @return the minimum size of a container to satisfy this contraint
	 */
	public Dimension containSize(Dimension componentSize) {

		int saveLeft = left;
		int saveTop = top;

		left -= adjustedAlignmentLeft(componentSize);
		top -= adjustedAlignmentTop(componentSize);

		Dimension dim = super.containSize(componentSize);

		left = saveLeft;
		top = saveTop;

		return dim;
	}
}
