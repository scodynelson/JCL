package jcl.lang.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.internal.number.IntegerStructImpl;
import jcl.type.ArrayType;
import jcl.type.LispType;
import jcl.type.SimpleArrayType;

/**
 * The {@link ArrayStructImpl} is the object representation of a Lisp 'array' type.
 *
 * @param <TYPE>
 * 		the type of the array contents
 */
public abstract class ArrayStructImpl<TYPE extends LispStruct> extends BuiltInClassStruct implements ArrayStruct<TYPE> {

	protected LispType elementType;

	protected boolean isAdjustable;

	protected ArrayStruct<TYPE> displacedTo;

	protected Integer displacedIndexOffset;

	protected ArrayStructImpl(final ArrayType arrayType, final LispType elementType, final boolean isAdjustable) {
		super(arrayType, null, null);

		this.elementType = elementType;
		this.isAdjustable = isAdjustable;
		displacedTo = null;
		displacedIndexOffset = 0;
	}

	protected ArrayStructImpl(final ArrayType arrayType, final LispType elementType,
	                          final ArrayStruct<TYPE> displacedTo, final Integer displacedIndexOffset,
	                          final boolean isAdjustable) {
		super(arrayType, null, null);

		this.elementType = elementType;
		this.isAdjustable = isAdjustable;
		this.displacedTo = displacedTo;
		this.displacedIndexOffset = displacedIndexOffset;
	}

	/**
	 * Gets the array type from the provided {@link #isAdjustable} value.
	 *
	 * @param isAdjustable
	 * 		whether or not the array is adjustable
	 *
	 * @return the matching array type for the provided {@link #isAdjustable} value
	 */
	protected static ArrayType getArrayType(final boolean isAdjustable) {
		return isAdjustable ? ArrayType.INSTANCE : SimpleArrayType.INSTANCE;
	}

	/**
	 * Determines if the provided {@code dimensionsToCheck} and {@code elementTypeToCheck} are valid for the provided
	 * {@code contentsToCheck}.
	 *
	 * @param dimensions
	 * 		the array dimensions to check
	 * @param elementType
	 * 		the array elementType to check
	 * @param initialContents
	 * 		the array contents to check
	 */
	protected static <TYPE extends LispStruct> List<TYPE> getValidContents(final List<Integer> dimensions,
	                                                                       final LispType elementType,
	                                                                       final SequenceStruct initialContents) {
		final int numberOfDimensions = dimensions.size();
		if (numberOfDimensions == 0) {
			return Collections.emptyList();
		}

		if (numberOfDimensions == 1) {
			final int dimension = dimensions.get(0);
			if (initialContents.length() == dimension) {
				return getValidContents(elementType, initialContents);
			} else {
				throw new SimpleErrorException(
						initialContents + " doesn't match array dimensions of #<" + elementType + ' ' + dimension + ">.");
			}
		}

		final List<TYPE> validContents = new ArrayList<>();

		final int dimension = dimensions.get(0);
		if (initialContents.length() == dimension) {
			final List<Integer> subDimension = dimensions.subList(1, numberOfDimensions);

			for (final LispStruct contentToCheck : initialContents) {
				if (!(contentToCheck instanceof SequenceStruct)) {
					throw new SimpleErrorException(
							initialContents + " doesn't match array dimensions of #<" + elementType + ' ' + dimension + ">.");
				}

				final SequenceStruct subContents = (SequenceStruct) contentToCheck;
				final List<TYPE> validSubContents = getValidContents(subDimension, elementType, subContents);
				validContents.addAll(validSubContents);
			}
		} else {
			throw new SimpleErrorException(
					initialContents + " doesn't match array dimensions of #<" + elementType + ' ' + dimension + ">.");
		}

		return validContents;
	}

	@SuppressWarnings("unchecked")
	private static <TYPE extends LispStruct> List<TYPE> getValidContents(final LispType elementType,
	                                                                     final SequenceStruct initialContents) {
		final List<TYPE> validContents = new ArrayList<>();

		for (final LispStruct current : initialContents) {
			final LispType currentType = current.getType();
			if (!currentType.equals(elementType) && !elementType.equals(currentType)) {
				throw new TypeErrorException(
						"Provided element " + current + " is not a subtype of the provided elementType " + elementType + '.');
			} else {
				validContents.add((TYPE) current);
			}
		}
		return validContents;
	}

	@Override
	public BooleanStruct adjustableArrayP() {
		return LispStructFactory.toBoolean(isAdjustable);
	}

	@Override
	public LispType arrayElementType() {
		return elementType;
	}

	@Override
	public ValuesStruct arrayDisplacement() {
		return (displacedTo == null)
		       ? ValuesStruct.valueOf(NILStruct.INSTANCE, IntegerStruct.ZERO)
		       : ValuesStruct.valueOf(displacedTo, IntegerStructImpl.valueOf(displacedIndexOffset));
	}
}
