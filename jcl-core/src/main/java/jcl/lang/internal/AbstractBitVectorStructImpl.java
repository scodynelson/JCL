package jcl.lang.internal;

import jcl.lang.BitVectorStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;

/**
 * Abstract base class for 'bit-vector' types.
 */
abstract class AbstractBitVectorStructImpl extends AbstractVectorStructImpl implements BitVectorStruct {

	/**
	 * Protected constructor, initializing the total size and the element-type as 'bit'.
	 *
	 * @param totalSize
	 * 		the total size of the 'bit-vector'
	 */
	protected AbstractBitVectorStructImpl(final IntegerStruct totalSize) {
		super(CommonLispSymbols.BIT, totalSize);
	}

	/**
	 * Validates and returns the provided object as a 'bit' fixnum value.
	 *
	 * @param object
	 * 		the object to validate
	 *
	 * @return the provided object as a 'bit' fixnum value
	 *
	 * @throws TypeErrorException
	 * 		if the provided object is not a valid 'bit' fixnum value
	 */
	protected static FixnumStruct getBit(final LispStruct object) {
		if (IntegerStruct.ZERO.eql(object)) {
			return IntegerStruct.ZERO;
		}
		if (IntegerStruct.ONE.eql(object)) {
			return IntegerStruct.ONE;
		}
		throw new TypeErrorException(object + " is not a bit type.");
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public abstract FixnumStruct aref(final IntegerStruct... subscripts);

	@Override
	public abstract FixnumStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts);

	@Override
	public abstract FixnumStruct rowMajorAref(final IntegerStruct index);

	@Override
	public abstract FixnumStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index);

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	public FixnumStruct elt(final IntegerStruct index) {
		final IntegerStruct indexInt = validateIndex(index);
		return aref(indexInt);
	}

	@Override
	public FixnumStruct setfElt(final LispStruct newElement, final IntegerStruct index) {
		final IntegerStruct indexInt = validateIndex(index);
		return setfAref(newElement, indexInt);
	}

	/*
	LISP-STRUCT
	 */

	@Override
	public LispStruct typeOf() {
		return ListStruct.toLispList(CommonLispSymbols.BIT_VECTOR, totalSize);
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.BIT_VECTOR;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.BIT_VECTOR) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.BIT_VECTOR) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
