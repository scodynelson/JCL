package jcl.lang;

/**
 * The {@link RatioStruct} is the object representation of a Lisp 'ratio' type.
 */
public interface RatioStruct extends RationalStruct {

	/*
	REAL-STRUCT
	 */

	@Override
	default RatioStruct rational() {
		return this;
	}

	/*
	NUMBER-STRUCT
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

	/*
	LISP-STRUCT
	 */

	@Override
	default boolean eql(final LispStruct object) {
		return eq(object) ||
				((object instanceof RatioStruct)
						&& ((RatioStruct) object).ap().equals(ap()));
	}
}
