package jcl.structs.numbers;

import jcl.structs.classes.BuiltInClassStruct;
import jcl.types.RandomState;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;

/**
 * The {@link RandomStateStruct} is the object representation of a Lisp 'random-state' type.
 */
public class RandomStateStruct extends BuiltInClassStruct {

	private static final int THREE = 3;
	private static final int FIFTY_FOUR = 54;
	private static final int CONST_A = 8378;
	private static final int CONST_B = 101010101;

	private static final BigInteger RANDOM_MAX = BigInteger.valueOf(FIFTY_FOUR);
	private static final BigInteger RANDOM_UPPER_BOUND = BigInteger.valueOf(Integer.MAX_VALUE - THREE);
	private static final BigInteger RANDOM_CONST_A = BigInteger.valueOf(CONST_A);
	private static final BigInteger RANDOM_CONST_B = BigInteger.valueOf(CONST_B);

	private static final BigInteger RANDOM_INTEGER_OVERLAP = BigInteger.valueOf(THREE);
	private static final BigInteger RANDOM_CHUNCK_LENGTH = BigInteger.valueOf(RANDOM_UPPER_BOUND.bitLength());

	private static final BigInteger SHIFT_AMOUNT = RANDOM_CHUNCK_LENGTH.subtract(RANDOM_INTEGER_OVERLAP);
	private static final int SHIFT_AMOUNT_AS_INT = SHIFT_AMOUNT.intValue();

	private static final int J_INITIAL_VALUE = 24;
	private BigInteger jValue = BigInteger.valueOf(J_INITIAL_VALUE);
	private BigInteger kValue = BigInteger.ZERO;

	private final List<BigInteger> seed;

	/**
	 * Public constructor.
	 */
	public RandomStateStruct() {
		super(RandomState.INSTANCE, null, null);
		seed = new ArrayList<>(RANDOM_MAX.intValue());

		BigInteger randSeed = BigInteger.ONE;
		for (int i = 0; i < RANDOM_MAX.intValue(); i++) {
			randSeed = randSeed.multiply(RANDOM_CONST_A).add(RANDOM_CONST_B).mod(RANDOM_UPPER_BOUND.add(BigInteger.ONE));
			seed.add(randSeed);
		}
	}

	/**
	 * This method retrieves a random integer from the internal random seed.
	 *
	 * @param limit
	 * 		the upper limit of the random integer
	 *
	 * @return the random {@link BigInteger}
	 */
	public BigInteger randomInteger(final BigInteger limit) {

		final BigInteger limitLength = BigInteger.valueOf(limit.bitLength());

		BigInteger bits = randomChunk();
		BigInteger count = limitLength.subtract(RANDOM_INTEGER_OVERLAP);
		while (BigInteger.ZERO.compareTo(count) <= 0) {
			final BigInteger shiftedBits = ash(bits);
			final BigInteger nextChunk = randomChunk();

			bits = shiftedBits.xor(nextChunk);
			count = count.subtract(SHIFT_AMOUNT);
		}

		return bits.remainder(limit);
	}

	/**
	 * This method retrieves a random float from the internal random seed.
	 *
	 * @param limit
	 * 		the upper limit of the random float
	 *
	 * @return the random {@link BigDecimal}
	 */
	public BigDecimal randomFloat(final BigDecimal limit) {

		final BigInteger randomInteger = randomInteger(limit.setScale(0, RoundingMode.CEILING).toBigInteger());
		BigDecimal randomDecimal = new BigDecimal(randomInteger);

		while (randomDecimal.compareTo(limit) >= 0) {
			randomDecimal = randomFloat(limit);
		}
		return randomDecimal;
	}

	/**
	 * This method obtains a random 'chunk', or integer value from the internal random seed.
	 *
	 * @return the random {@link BigInteger} 'chunk'
	 */
	private BigInteger randomChunk() {
		final BigInteger tempJ = jValue;
		final BigInteger tempK = kValue;

		// If J part
		jValue = tempJ.equals(BigInteger.ZERO) ? RANDOM_MAX : tempJ.subtract(BigInteger.ONE);

		// Seed at new jValue
		final BigInteger seedAtNewJ = seed.get(kValue.intValue());

		// If K part
		kValue = tempK.equals(BigInteger.ZERO) ? RANDOM_MAX : tempJ.subtract(BigInteger.ONE);

		// Seed at new kValue
		final BigInteger seedAtNewK = seed.get(tempK.intValue());

		// A part
		final BigInteger a = seedAtNewJ.subtract(seedAtNewK);

		// If -a part
		final BigInteger ifAPart = (tempJ.compareTo(BigInteger.ZERO) == -1) ? BigInteger.ZERO.subtract(a) : RANDOM_UPPER_BOUND.subtract(a);

		seed.set(tempK.intValue(), a);

		return ifAPart;
	}

	/**
	 * This method is a bit shifting method depending on the sign of the shift value.
	 *
	 * @param bits
	 * 		the bits to shift
	 *
	 * @return a new shifted {@link BigInteger}
	 */
	private static BigInteger ash(final BigInteger bits) {
		return (SHIFT_AMOUNT_AS_INT <= 0) ? bits.shiftRight(SHIFT_AMOUNT_AS_INT) : bits.shiftLeft(SHIFT_AMOUNT_AS_INT);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
