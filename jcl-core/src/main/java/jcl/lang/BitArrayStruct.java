package jcl.lang;

import jcl.lang.condition.exception.TypeErrorException;
import jcl.type.LispType;
import jcl.type.SimpleArrayType;

/**
 * The {@link BitArrayStruct} is the object representation of a Lisp 'bit-array' type.
 */
public interface BitArrayStruct extends ArrayStruct<IntegerStruct> {

	default IntegerStruct bit(final IntegerStruct... subscripts) {
		return aref(subscripts);
	}

	default IntegerStruct setfBit(final IntegerStruct newElement, final IntegerStruct... subscripts) {
		return setfAref(newElement, subscripts);
	}

	default IntegerStruct sbit(final IntegerStruct... subscripts) {
		final LispType type = getType();
		if (SimpleArrayType.INSTANCE.equals(type)) {
			return aref(subscripts);
		}
		throw new TypeErrorException(
				"The value " + this + " is not of the expected type " + SimpleArrayType.INSTANCE + '.');
	}

	default IntegerStruct setfSvref(final IntegerStruct newElement, final IntegerStruct... subscripts) {
		final LispType type = getType();
		if (SimpleArrayType.INSTANCE.equals(type)) {
			return setfAref(newElement, subscripts);
		}
		throw new TypeErrorException(
				"The value " + this + " is not of the expected type " + SimpleArrayType.INSTANCE + '.');
	}
}
