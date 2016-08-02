package jcl.lang;

/**
 * The {@link RatioStruct} is the object representation of a Lisp 'ratio' type.
 */
public interface RatioStruct extends RationalStruct {

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
}
