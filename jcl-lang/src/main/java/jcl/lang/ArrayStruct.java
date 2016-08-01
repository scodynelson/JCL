package jcl.lang;

import java.util.List;

import jcl.type.LispType;

/**
 * The {@link ArrayStruct} is the object representation of a Lisp 'array' type.
 *
 * @param <TYPE>
 * 		the type of the array contents
 */
public interface ArrayStruct<TYPE extends LispStruct> extends LispStruct {

	/**
	 * Gets the array contents.
	 *
	 * @return array contents
	 */
	List<TYPE> getContents();

	/**
	 * Sets the array contents.
	 *
	 * @param contents
	 * 		new array contents
	 */
	void setContents(final List<TYPE> contents);

	/**
	 * Gets the array dimensions.
	 *
	 * @return array dimensions
	 */
	List<Integer> getDimensions();

	/**
	 * Sets the array dimensions.
	 *
	 * @param dimensions
	 * 		new array dimensions
	 */
	void setDimensions(final List<Integer> dimensions);

	/**
	 * Gets the array's total size.
	 *
	 * @return array's total size
	 */
	int getTotalSize();

	/**
	 * Gets the array rank.
	 *
	 * @return array rank
	 */
	int getRank();

	/**
	 * Sets the array rank.
	 *
	 * @param rank
	 * 		new array rank
	 */
	void setRank(final int rank);

	/**
	 * Gets the array elementType.
	 *
	 * @return array elementType
	 */
	LispType getElementType();

	/**
	 * Sets the array elementType.
	 *
	 * @param elementType
	 * 		new array elementType
	 */
	void setElementType(final LispType elementType);

	/**
	 * Gets the array contents.
	 *
	 * @return array contents
	 */
	boolean isAdjustable();

	/**
	 * Setter for array {@link #isAdjustable} property.
	 *
	 * @param isAdjustable
	 * 		new array {@link #isAdjustable} property value
	 */
	void setAdjustable(final boolean isAdjustable);

	/**
	 * Retrieves the element at the provided {@code index} location.
	 *
	 * @param index
	 * 		the index location of the element to retrieve
	 *
	 * @return the retrieve element at the provided {@code index} location
	 */
	TYPE getElementAt(final int index);

	/**
	 * Sets the element at the provide {@code index} location to the provided {@code newValue} element.
	 *
	 * @param index
	 * 		the index location of the element to set
	 * @param newValue
	 * 		the element to set at the index location
	 */
	void setElementAt(final int index, final TYPE newValue);
}
