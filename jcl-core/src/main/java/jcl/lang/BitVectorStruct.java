package jcl.lang;

import java.util.List;

import jcl.lang.internal.ComplexBitVectorStructImpl;
import jcl.lang.internal.SimpleBitVectorStructImpl;

/**
 * The {@link BitVectorStruct} is the object representation of a Lisp 'bit-vector' type.
 */
public interface BitVectorStruct extends VectorStruct, BitArrayStruct {

	/**
	 * Returns a new BitVectorStruct representation of the provided size and contents.
	 *
	 * @param size
	 * 		the bit-vector size
	 * @param contents
	 * 		the bit-vector contents
	 *
	 * @return a new BitVectorStruct representation of the provided size and contents
	 */
	static BitVectorStruct toLispBitVector(final IntegerStruct size, final List<FixnumStruct> contents) {
		return new SimpleBitVectorStructImpl(size, contents);
	}

	/**
	 * Returns a new BitVectorStruct representation of the provided size, contents, adjustable, and fillPointer.
	 *
	 * @param size
	 * 		the bit-vector size
	 * @param contents
	 * 		the bit-vector contents
	 * @param adjustable
	 * 		whether or not the bit-vector is adjustable
	 * @param fillPointer
	 * 		the bit-vector fillPointer
	 *
	 * @return a new BitVectorStruct representation of the provided size, contents, adjustable, and fillPointer
	 */
	static BitVectorStruct toLispBitVector(final IntegerStruct size, final List<FixnumStruct> contents,
	                                       final BooleanStruct adjustable, final IntegerStruct fillPointer) {
		final BooleanStruct realAdjustable = (adjustable == null) ? NILStruct.INSTANCE : adjustable;
		return new ComplexBitVectorStructImpl(size, contents, realAdjustable, fillPointer);
	}

	/**
	 * Returns a new BitVectorStruct representation of the provided size, displacedTo, displacedIndexOffset, adjustable,
	 * and fillPointer.
	 *
	 * @param size
	 * 		the bit-vector size
	 * @param displacedTo
	 * 		the array structure that this array structure will be displaced to for content values
	 * @param displacedIndexOffset
	 * 		the offset of the index lookup for the content value into the displaced array structure
	 * @param adjustable
	 * 		whether or not the bit-vector is adjustable
	 * @param fillPointer
	 * 		the bit-vector fillPointer
	 *
	 * @return a new BitVectorStruct representation of the provided size, displacedTo, displacedIndexOffset, adjustable,
	 * 		and fillPointer
	 */
	static BitVectorStruct toLispBitVector(final IntegerStruct size,
	                                       final ArrayStruct displacedTo, final IntegerStruct displacedIndexOffset,
	                                       final BooleanStruct adjustable, final IntegerStruct fillPointer) {
		final BooleanStruct realAdjustable = (adjustable == null) ? NILStruct.INSTANCE : adjustable;
		return new ComplexBitVectorStructImpl(size, displacedTo, displacedIndexOffset, realAdjustable, fillPointer);
	}

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	FixnumStruct elt(final IntegerStruct index);

	@Override
	FixnumStruct setfElt(final LispStruct newElement, final IntegerStruct index);

	@Override
	BitVectorStruct reverse();

	@Override
	BitVectorStruct nReverse();

	/*
	LISP-STRUCT
	 */

	@Override
	default boolean equal(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof BitVectorStruct) {
			final BitVectorStruct v = (BitVectorStruct) object;

			final IntegerStruct thisLength = length();
			if (!thisLength.eql(v.length())) {
				return false;
			}
			for (int i = 0; i < thisLength.toJavaInt(); i++) {
				final IntegerStruct index = IntegerStruct.toLispInteger(i);
				if (!rowMajorAref(index).eql(v.rowMajorAref(index))) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	@Override
	default boolean equalp(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof BitVectorStruct) {
			final BitVectorStruct v = (BitVectorStruct) object;

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
		if (object instanceof StringStruct) {
			return false;
		}
		if (object instanceof VectorStruct) {
			return object.equalp(this);
		}
		return false;
	}
}
