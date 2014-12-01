package jcl.arrays;

import jcl.LispStruct;
import jcl.LispType;
import jcl.classes.BuiltInClassStruct;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.printer.PrinterVariables;
import jcl.types.Array;
import jcl.types.SimpleArray;
import jcl.types.T;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.List;

/**
 * The {@link ArrayStruct} is the object representation of a Lisp 'array' type.
 *
 * @param <TYPE>
 * 		the type of the array contents
 */
public class ArrayStruct<TYPE extends LispStruct> extends BuiltInClassStruct {

	protected List<TYPE> contents;
	protected List<Integer> dimensions;
	protected int totalSize;
	protected int rank;
	protected LispType elementType;
	protected boolean isAdjustable;

	/**
	 * Public constructor.
	 *
	 * @param dimensions
	 * 		the array dimensions
	 * @param contents
	 * 		the array contents
	 */
	public ArrayStruct(final List<Integer> dimensions, final List<TYPE> contents) {
		this(SimpleArray.INSTANCE, dimensions, contents, T.INSTANCE, false);
	}

	/**
	 * Public constructor.
	 *
	 * @param dimensions
	 * 		the array dimensions
	 * @param contents
	 * 		the array contents
	 * @param elementType
	 * 		the array elementType
	 * @param isAdjustable
	 * 		whether or not the array is adjustable
	 */
	public ArrayStruct(final List<Integer> dimensions, final List<TYPE> contents, final LispType elementType, final boolean isAdjustable) {
		this(getArrayType(isAdjustable), dimensions, contents, elementType, isAdjustable);
	}

	/**
	 * Protected constructor.
	 *
	 * @param arrayType
	 * 		the array type
	 * @param dimensions
	 * 		the array dimensions
	 * @param contents
	 * 		the array contents
	 * @param elementType
	 * 		the array elementType
	 * @param isAdjustable
	 * 		whether or not the array is adjustable
	 */
	protected ArrayStruct(final Array arrayType,
	                      final List<Integer> dimensions, final List<TYPE> contents, final LispType elementType, final boolean isAdjustable) {
		super(arrayType, null, null);

		// Check input data
		areContentsValidForDimensionsAndElementType(dimensions, elementType, contents);

		this.contents = new ArrayList<>(contents);
		this.dimensions = dimensions;
		updateTotalSize();

		rank = dimensions.size();

		this.elementType = elementType;
		this.isAdjustable = isAdjustable;
	}

	/**
	 * Gets the array type from the provided {@link #isAdjustable} value.
	 *
	 * @param isAdjustable
	 * 		whether or not the array is adjustable
	 *
	 * @return the matching array type for the provided {@link #isAdjustable} value
	 */
	private static Array getArrayType(final boolean isAdjustable) {
		return isAdjustable ? Array.INSTANCE : SimpleArray.INSTANCE;
	}

	/**
	 * Determines if the provided {@code dimensionsToCheck} and {@code elementTypeToCheck} are valid for the provided
	 * {@code contentsToCheck}.
	 *
	 * @param dimensionsToCheck
	 * 		the array dimensions to check
	 * @param elementTypeToCheck
	 * 		the array elementType to check
	 * @param contentsToCheck
	 * 		the array contents to check
	 */
	private void areContentsValidForDimensionsAndElementType(final List<Integer> dimensionsToCheck, final LispType elementTypeToCheck,
	                                                         final List<TYPE> contentsToCheck) {

		int totalNumberOfElements = 0;
		for (final int dimension : dimensionsToCheck) {
			totalNumberOfElements += dimension;
		}

		final int contentsSize = contentsToCheck.size();
		if (contentsSize != totalNumberOfElements) {
			throw new SimpleErrorException(contentsToCheck + " doesn't match array dimensions of #<" + elementTypeToCheck + ' ' + contentsSize + ">.");
		}

		for (final TYPE current : contentsToCheck) {
			if (!current.getType().equals(elementTypeToCheck) && !elementTypeToCheck.equals(current.getType())) {
				throw new TypeErrorException("Provided element " + current + " is not a subtype of the provided elementType " + elementTypeToCheck + '.');
			}
		}
	}

	/**
	 * Getter for array {@link #contents} property.
	 *
	 * @return array {@link #contents} property
	 */
	public List<TYPE> getContents() {
		return contents;
	}

	/**
	 * Setter for array {@link #contents} property.
	 *
	 * @param contents
	 * 		new array {@link #contents} property value
	 */
	public void setContents(final List<TYPE> contents) {
		this.contents = new ArrayList<>(contents);
	}

	/**
	 * Getter for array {@link #dimensions} property.
	 *
	 * @return array {@link #dimensions} property
	 */
	public List<Integer> getDimensions() {
		return dimensions;
	}

	/**
	 * Setter for array {@link #dimensions} property.
	 *
	 * @param dimensions
	 * 		new array {@link #dimensions} property value
	 */
	public void setDimensions(final List<Integer> dimensions) {
		this.dimensions = dimensions;
		updateTotalSize();
	}

	/**
	 * Getter for array {@link #totalSize} property.
	 *
	 * @return array {@link #totalSize} property
	 */
	public int getTotalSize() {
		return totalSize;
	}

	/**
	 * Getter for array {@link #rank} property.
	 *
	 * @return array {@link #rank} property
	 */
	public int getRank() {
		return rank;
	}

	/**
	 * Setter for array {@link #rank} property.
	 *
	 * @param rank
	 * 		new array {@link #rank} property value
	 */
	public void setRank(final int rank) {
		this.rank = rank;
	}

	/**
	 * Getter for array {@link #elementType} property.
	 *
	 * @return array {@link #elementType} property
	 */
	public LispType getElementType() {
		return elementType;
	}

	/**
	 * Setter for array {@link #elementType} property.
	 *
	 * @param elementType
	 * 		new array {@link #elementType} property value
	 */
	public void setElementType(final LispType elementType) {
		this.elementType = elementType;
	}

	/**
	 * Getter for array {@link #isAdjustable} property.
	 *
	 * @return array {@link #isAdjustable} property
	 */
	public boolean isAdjustable() {
		return isAdjustable;
	}

	/**
	 * Setter for array {@link #isAdjustable} property.
	 *
	 * @param isAdjustable
	 * 		new array {@link #isAdjustable} property value
	 */
	public void setAdjustable(final boolean isAdjustable) {
		this.isAdjustable = isAdjustable;
	}

	/**
	 * Retrieves the element at the provided {@code index} location.
	 *
	 * @param index
	 * 		the index location of the element to retrieve
	 *
	 * @return the retrieve element at the provided {@code index} location
	 */
	public TYPE getElementAt(final int index) {
		return contents.get(index);
	}

	/**
	 * Sets the element at the provide {@code index} location to the provided {@code newValue} element.
	 *
	 * @param index
	 * 		the index location of the element to set
	 * @param newValue
	 * 		the element to set at the index location
	 */
	public void setElementAt(final int index, final TYPE newValue) {
		for (int i = contents.size(); i <= index; i++) {
			contents.add(null);
		}
		contents.set(index, newValue);
	}

	/**
	 * Calculates and updates the {@link #totalSize} of the array based on the {@link
	 * #dimensions} property.
	 */
	private void updateTotalSize() {
		totalSize = 0;
		for (final Integer dimension : dimensions) {
			totalSize += dimension;
		}
	}

	@Override
	public String printStruct() {
		// TODO: Ignoring *PRINT-LEVEL* and *PRINT-LENGTH*

		final boolean printArray = PrinterVariables.PRINT_ARRAY.getValue().booleanValue();
		final boolean printReadably = PrinterVariables.PRINT_READABLY.getValue().booleanValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printArray || printReadably) {
			stringBuilder.append('#');
			stringBuilder.append(rank);
			stringBuilder.append("#(");

			for (int i = 0; i < totalSize; i++) {
				final LispStruct lispStruct = contents.get(i);
				stringBuilder.append(lispStruct.printStruct());

				if (i < (totalSize - 1)) {
					stringBuilder.append(' ');
				}
			}

			stringBuilder.append(')');
		} else {
			final String typeClassName = getType().getClass().getName().toUpperCase();

			stringBuilder.append("#<");
			stringBuilder.append(typeClassName);
			stringBuilder.append(' ');

			for (int i = 0; i < dimensions.size(); i++) {
				stringBuilder.append(dimensions.get(i));

				if ((i + 1) != dimensions.size()) {
					stringBuilder.append('x');
				}
			}

			stringBuilder.append(" type ");
			final String elementTypeClassName = elementType.getClass().getName().toUpperCase();
			stringBuilder.append(elementTypeClassName);

			if (isAdjustable) {
				stringBuilder.append(" adjustable");
			}

			stringBuilder.append('>');
		}

		return stringBuilder.toString();
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
