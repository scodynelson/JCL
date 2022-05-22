package jcl.lang;

import java.util.List;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.ComplexVectorStructImpl;
import jcl.lang.internal.NILVectorStructImpl;
import jcl.lang.internal.SimpleVectorStructImpl;

/**
 * The {@link VectorStruct} is the object representation of a Lisp 'vector' type.
 */
public interface VectorStruct extends ArrayStruct, SequenceStruct {

	/**
	 * Gets the vector's fill-pointer.
	 *
	 * @return vector's fill-pointer
	 */
	IntegerStruct fillPointer();

	/**
	 * Sets the vector's fill-pointer.
	 *
	 * @param fillPointer
	 * 		new vector fill-pointer
	 *
	 * @return the new fill-pointer value
	 */
	IntegerStruct setfFillPointer(final IntegerStruct fillPointer);

	/**
	 * Pops the element at the fill-pointer index and decreases the fill-pointer by 1.
	 *
	 * @return the element popped from the fill-pointer index
	 *
	 * @throws ErrorException
	 * 		if the vector has no fill-pointer or the fill-pointer is 0
	 */
	LispStruct vectorPop();

	/**
	 * Pushes the provided {@code element} into the current fill-pointer index.
	 *
	 * @param newElement
	 * 		the element to push into the vector
	 *
	 * @return the location of the newly added element
	 *
	 * @throws TypeErrorException
	 * 		if the vector has no fill-pointer
	 */
	LispStruct vectorPush(final LispStruct newElement);

	/**
	 * Pushes the provided {@code element} into the current fill-pointer index and extends the vector to the
	 * current size of the contents plus the provided {@code extensionAmount}.
	 *
	 * @param newElement
	 * 		the element to push into the vector
	 * @param extension
	 * 		the amount to extend the vector when pushing
	 *
	 * @return the location of the newly added element
	 *
	 * @throws TypeErrorException
	 * 		if the vector has no fill-pointer or the vector is not adjustable
	 */
	IntegerStruct vectorPushExtend(final LispStruct newElement, final IntegerStruct extension);

	/**
	 * Returns a new VectorStruct representation of the provided size.
	 *
	 * @param size
	 * 		the vector size
	 *
	 * @return a new VectorStruct representation of the provided size
	 */
	static VectorStruct toLispVector(final IntegerStruct size) {
		return new NILVectorStructImpl(size);
	}

	/**
	 * Returns a new VectorStruct representation of the provided size, element-type, and contents.
	 *
	 * @param size
	 * 		the vector size
	 * @param elementType
	 * 		the vector elementType
	 * @param contents
	 * 		the vector contents
	 *
	 * @return a new VectorStruct representation of the provided size, element-type, and contents
	 */
	static VectorStruct toLispVector(final IntegerStruct size, final SymbolStruct elementType,
	                                 final List<LispStruct> contents) {
		return new SimpleVectorStructImpl(size, elementType, contents);
	}

	/**
	 * Returns a new VectorStruct representation of the provided size, element-type, contents, adjustable, and
	 * fillPointer.
	 *
	 * @param size
	 * 		the vector size
	 * @param contents
	 * 		the vector contents
	 * @param elementType
	 * 		the vector elementType
	 * @param adjustable
	 * 		whether or not the vector is adjustable
	 * @param fillPointer
	 * 		the vector fillPointer
	 *
	 * @return a new VectorStruct representation of the provided size, element-type, contents, adjustable, and fillPointer
	 */
	static VectorStruct toLispVector(final IntegerStruct size, final SymbolStruct elementType,
	                                 final List<LispStruct> contents, final BooleanStruct adjustable,
	                                 final IntegerStruct fillPointer) {
		final BooleanStruct realAdjustable = (adjustable == null) ? NILStruct.INSTANCE : adjustable;
		return new ComplexVectorStructImpl(size, elementType, contents, realAdjustable, fillPointer);
	}

	/**
	 * Returns a new VectorStruct representation of the provided size, element-type, displacedTo, displacedIndexOffset,
	 * adjustable, and fillPointer.
	 *
	 * @param size
	 * 		the vector size
	 * @param displacedTo
	 * 		the array structure that this array structure will be displaced to for content values
	 * @param displacedIndexOffset
	 * 		the offset of the index lookup for the content value into the displaced array structure
	 * @param elementType
	 * 		the vector elementType
	 * @param adjustable
	 * 		whether or not the vector is adjustable
	 * @param fillPointer
	 * 		the vector fillPointer
	 *
	 * @return a new VectorStruct representation of the provided size, element-type, displacedTo, displacedIndexOffset,
	 * 		adjustable, and fillPointer
	 */
	static VectorStruct toLispVector(final IntegerStruct size, final SymbolStruct elementType,
	                                 final ArrayStruct displacedTo, final IntegerStruct displacedIndexOffset,
	                                 final BooleanStruct adjustable, final IntegerStruct fillPointer) {
		final BooleanStruct realAdjustable = (adjustable == null) ? NILStruct.INSTANCE : adjustable;
		return new ComplexVectorStructImpl(
				size, elementType, displacedTo, displacedIndexOffset, realAdjustable, fillPointer
		);
	}

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	VectorStruct reverse();

	@Override
	VectorStruct nReverse();

	/*
	LISP-STRUCT
	 */

	@Override
	default boolean equalp(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof VectorStruct) {
			final VectorStruct v = (VectorStruct) object;

			final IntegerStruct thisLength = length();
			if (!thisLength.eql(v.length())) {
				return false;
			}
			for (int i = 0; i < thisLength.toJavaInt(); i++) {
				final IntegerStruct index = IntegerStruct.toLispInteger(i);
				if (!rowMajorAref(index).equalp(v.rowMajorAref(index))) {
					return false;
				}
			}
			return true;
		}
		if (object instanceof ArrayStruct) {
			return object.equalp(this);
		}
		return false;
	}
}
