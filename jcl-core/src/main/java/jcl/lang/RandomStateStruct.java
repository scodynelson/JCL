package jcl.lang;

import java.security.SecureRandom;

import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.number.RandomStateStructImpl;
import jcl.lang.statics.NumberVariables;

/**
 * The {@link RandomStateStruct} is the object representation of a Lisp 'random-state' type.
 */
public interface RandomStateStruct extends LispStruct {

	/**
	 * Returns the underlying {@link SecureRandom} used to seed the random generated values.
	 *
	 * @return the underlying {@link SecureRandom} used to seed the random generated values
	 */
	SecureRandom randomGenerator();

	/**
	 * Returns a pseudo-random number that is a non-negative number less than limit and of the same type as limit.
	 *
	 * @param limit
	 * 		a positive integer, or a positive float, to be used as the limit value for the result
	 *
	 * @return a pseudo-random number
	 */
	RealStruct random(final RealStruct limit);

	/**
	 * Returns a new RandomStateStruct.
	 *
	 * @return a new RandomStateStruct
	 */
	static RandomStateStruct toLispRandomState() {
		return new RandomStateStructImpl();
	}

	/**
	 * Returns a new RandomStateStruct based on the provided state value, which can either be another RandomStateStruct,
	 * T, or NIL.
	 *
	 * @param state
	 * 		the state used to create a new RandomStateStruct
	 *
	 * @return a new RandomStateStruct
	 *
	 * @throws TypeErrorException
	 * 		if the provided {@code state} is not a a RANDOM-STATE, T or NIL
	 */
	static RandomStateStruct makeRandomState(final LispStruct state) {
		if (state instanceof RandomStateStruct) {
			return new RandomStateStructImpl((RandomStateStruct) state);
		}
		if (NILStruct.INSTANCE.eq(state)) {
			return new RandomStateStructImpl(NumberVariables.RANDOM_STATE.getVariableValue());
		}
		if (TStruct.INSTANCE.eq(state)) {
			return new RandomStateStructImpl();
		}
		throw new TypeErrorException("Argument is not a RANDOM-STATE, T or NIL: " + state);
	}
}
