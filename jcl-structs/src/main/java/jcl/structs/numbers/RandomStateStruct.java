package jcl.structs.numbers;

import jcl.structs.classes.BuiltInClassStruct;
import jcl.types.numbers.RandomState;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.ArrayList;

/**
 * The {@code RandomStateStruct} is the object representation of a Lisp 'random-state' type.
 */
public class RandomStateStruct extends BuiltInClassStruct {

	private static final BigInteger RANDOM_MAX = BigInteger.valueOf(54);
	private static final BigInteger RANDOM_UPPER_BOUND = BigInteger.valueOf(Integer.MAX_VALUE - 3);
	private static final BigInteger RANDOM_CONST_A = BigInteger.valueOf(8373);
	private static final BigInteger RANDOM_CONST_C = BigInteger.valueOf(101010101);

	private static final BigInteger RANDOM_INTEGER_OVERLAP = BigInteger.valueOf(3);
	private static final BigInteger RANDOM_INTEGER_EXTRA_BITS = BigInteger.TEN;
	private static final BigInteger RANDOM_CHUNCK_LENGTH = BigInteger.valueOf(RANDOM_UPPER_BOUND.bitLength());

	private static final int J_INITIAL_VALUE = 24;
	private BigInteger j_value = BigInteger.valueOf(J_INITIAL_VALUE);
	private BigInteger k_value = BigInteger.ZERO;

	private final ArrayList<BigInteger> seed;

	/**
	 * Public constructor.
	 */
	public RandomStateStruct() {
		super(RandomState.INSTANCE, null, null);
		seed = new ArrayList<>(RANDOM_MAX.intValue());

		BigInteger randSeed = BigInteger.ONE;
		for (int i = 0; i < RANDOM_MAX.intValue(); i++) {
			randSeed = randSeed.multiply(RANDOM_CONST_A).add(RANDOM_CONST_C).mod(RANDOM_UPPER_BOUND.add(BigInteger.ONE));
			seed.add(randSeed);
		}
	}

	/**
	 * This method retrieves a random integer from the internal random seed.
	 *
	 * @param limit the upper limit of the random integer
	 * @return the random {@code BigInteger}
	 */
	public BigInteger randomInteger(final BigInteger limit) {

		final BigInteger shift = RANDOM_CHUNCK_LENGTH.subtract(RANDOM_INTEGER_OVERLAP);

		final BigInteger limitLength = BigInteger.valueOf(limit.bitLength());

		BigInteger bits = randomChunk();
		BigInteger count = limitLength.subtract(RANDOM_INTEGER_OVERLAP);
		while (BigInteger.ZERO.compareTo(count) <= 0) {
			final BigInteger shiftedBits = ash(bits, shift);
			final BigInteger nextChunk = randomChunk();

			bits = shiftedBits.xor(nextChunk);
			count = count.subtract(shift);
		}

		return bits.remainder(limit);
	}

	/**
	 * This method retrieves a random float from the internal random seed.
	 *
	 * @param limit the upper limit of the random float
	 * @return the random {@code BigDecimal}
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
	 * @return the random {@code BigInteger} 'chunk'
	 */
	private BigInteger randomChunk() {
		final BigInteger tempJ = j_value;
		final BigInteger tempK = k_value;

		// If J part
		j_value = tempJ.equals(BigInteger.ZERO) ? RANDOM_MAX : tempJ.subtract(BigInteger.ONE);

		// Seed at new j_value
		final BigInteger seedAtNewJ = seed.get(k_value.intValue());

		// If K part
		k_value = tempK.equals(BigInteger.ZERO) ? RANDOM_MAX : tempJ.subtract(BigInteger.ONE);

		// Seed at new k_value
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
	 * @param bits  the bits to shift
	 * @param shift the shift value
	 * @return a new shifted {@code BigInteger}
	 */
	private static BigInteger ash(final BigInteger bits, final BigInteger shift) {
		final BigInteger shiftedBits;
		if (BigInteger.ZERO.compareTo(shift) <= 0) {
			shiftedBits = bits.shiftRight(shift.intValue());
		} else {
			shiftedBits = bits.shiftLeft(shift.intValue());
		}
		return shiftedBits;
	}

	@Override
	public String toString() {
		return "RandomStateStruct{"
				+ "j=" + j_value
				+ ", k=" + k_value
				+ ", seed=" + seed
				+ '}';
	}
}
