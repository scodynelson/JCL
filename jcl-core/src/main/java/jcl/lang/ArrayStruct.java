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

//	IntegerStruct dimension(final int axisNumber);

	/**
	 * Gets the array contents.
	 *
	 * @return array contents
	 */
	List<TYPE> getContents();

	/**
	 * Gets the array dimensions.
	 *
	 * @return array dimensions
	 */
	List<Integer> getDimensions();

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
	 * Gets the array elementType.
	 *
	 * @return array elementType
	 */
	LispType getElementType();

	/**
	 * Gets the array contents.
	 *
	 * @return array contents
	 */
	boolean isAdjustable();

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
	 * Retrieves the element at the provided {@code index} location.
	 *
	 * @param indicies
	 * 		the index location of the element to retrieve
	 *
	 * @return the retrieve element at the provided {@code index} location
	 */
	TYPE getElementAt(final int... indicies);

	/**
	 * Sets the element at the provide {@code index} location to the provided {@code newValue} element.
	 *
	 * @param index
	 * 		the index location of the element to set
	 * @param newValue
	 * 		the element to set at the index location
	 */
	void setElementAt(final int index, final TYPE newValue);

	ArrayStruct<TYPE> getDisplacedTo();

	Integer getDisplacedIndexOffset();
}
