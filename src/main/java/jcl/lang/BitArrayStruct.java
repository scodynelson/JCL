package jcl.lang;

import jcl.lang.condition.exception.TypeErrorException;

/**
 * The {@link BitArrayStruct} is the object representation of a Lisp 'bit-array' type.
 */
public interface BitArrayStruct extends ArrayStruct {

	/**
	 * Retrieves the {@link FixnumStruct} at the provided position determined by the provided {@code subscripts}.
	 *
	 * @param subscripts
	 * 		the position of the {@link FixnumStruct} to retrieve
	 *
	 * @return the {@link FixnumStruct} at the provided subscripts
	 */
	FixnumStruct bit(final IntegerStruct... subscripts);

	/**
	 * Sets the bit of bit-array specified by index to a new value.
	 *
	 * @param newBit
	 * 		new bit value
	 * @param subscripts
	 * 		the position of the {@link FixnumStruct} to retrieve
	 *
	 * @return the new bit
	 */
	FixnumStruct setfBit(final FixnumStruct newBit, final IntegerStruct... subscripts);

	/**
	 * Retrieves the {@link FixnumStruct} at the provided position determined by the provided {@code subscripts}.
	 *
	 * @param subscripts
	 * 		the position of the {@link FixnumStruct} to retrieve
	 *
	 * @return the {@link FixnumStruct} at the provided subscripts
	 */
	default FixnumStruct sbit(final IntegerStruct... subscripts) {
		throw new TypeErrorException("Invalid BIT-ARRAY type: " + this);
	}

	/**
	 * Sets the bit of bit-array specified by index to a new value. String must be a "simple-string" type.
	 *
	 * @param newBit
	 * 		new bit value
	 * @param subscripts
	 * 		the position of the {@link FixnumStruct} to retrieve
	 *
	 * @return the new bit
	 */
	FixnumStruct setfSbit(final FixnumStruct newBit, final IntegerStruct... subscripts);

	/*
	ARRAY-STRUCT
	 */

	@Override
	FixnumStruct aref(final IntegerStruct... subscripts);

	@Override
	FixnumStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts);

	@Override
	FixnumStruct rowMajorAref(final IntegerStruct index);

	@Override
	FixnumStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index);
}
