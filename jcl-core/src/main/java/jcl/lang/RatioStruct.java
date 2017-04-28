package jcl.lang;

import org.apache.commons.math3.fraction.BigFraction;

/**
 * The {@link RatioStruct} is the object representation of a Lisp 'ratio' type.
 */
public interface RatioStruct extends RationalStruct {

	BigFraction toBigFraction();

	/*
		RealStruct
	 */

	@Override
	default RatioStruct rational() {
		return this;
	}

	/*
		NumberStruct
	 */

	@Override
	RatioStruct abs();

	@Override
	default RatioStruct realPart() {
		return this;
	}

	@Override
	default RatioStruct conjugate() {
		return this;
	}

	@Override
	RatioStruct negation();

	@Override
	default boolean eql(final LispStruct object) {
		return eq(object) ||
				((object instanceof RatioStruct)
						&& ((RatioStruct) object).ap().equals(ap()));
	}
}
