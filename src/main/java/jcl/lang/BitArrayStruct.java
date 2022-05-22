package jcl.lang;

/**
 * The {@link BitArrayStruct} is the object representation of a Lisp 'bit-array' type.
 */
public interface BitArrayStruct extends ArrayStruct {

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
