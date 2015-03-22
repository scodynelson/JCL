/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;

import jcl.classes.BuiltInClassStruct;
import jcl.types.RandomState;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link RandomStateStruct} is the object representation of a Lisp 'random-state' type.
 */
public class RandomStateStruct extends BuiltInClassStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 457336984118960936L;

	/**
	 * Int constant for overlap.
	 */
	private static final int OVERLAP = 3;

	/**
	 * Int constant for max.
	 */
	private static final int MAX = 54;

	/**
	 * Int constant for first constant.
	 */
	private static final int CONST_A = 8378;

	/**
	 * Int constant for second constant.
	 */
	private static final int CONST_B = 101010101;

	/**
	 * {@link BigInteger} constant for random overlap.
	 */
	private static final BigInteger RANDOM_OVERLAP = BigInteger.valueOf(OVERLAP);

	/**
	 * {@link BigInteger} constant for random max.
	 */
	private static final BigInteger RANDOM_MAX = BigInteger.valueOf(MAX);

	/**
	 * {@link BigInteger} constant for first random.
	 */
	private static final BigInteger RANDOM_CONST_A = BigInteger.valueOf(CONST_A);

	/**
	 * {@link BigInteger} constant for second random.
	 */
	private static final BigInteger RANDOM_CONST_B = BigInteger.valueOf(CONST_B);

	/**
	 * {@link BigInteger} constant for random upper bound.
	 */
	private static final BigInteger RANDOM_UPPER_BOUND = BigInteger.valueOf(Integer.MAX_VALUE - OVERLAP);

	/**
	 * {@link BigInteger} constant for random chunk length.
	 */
	private static final BigInteger RANDOM_CHUNK_LENGTH = BigInteger.valueOf(RANDOM_UPPER_BOUND.bitLength());

	/**
	 * {@link BigInteger} constant for shift amount.
	 */
	private static final BigInteger BIT_SHIFT_AMOUNT = RANDOM_CHUNK_LENGTH.subtract(RANDOM_OVERLAP);

	/**
	 * Int constant shift amount.
	 */
	private static final int BIT_SHIFT_AMOUNT_AS_INT = BIT_SHIFT_AMOUNT.intValueExact();

	/**
	 * Int constant for the 'J' initial value.
	 */
	private static final int J_INITIAL_VALUE = 24;

	/**
	 * Holds the 'Seed' values to be used in the random algorithm.
	 */
	private final List<BigInteger> seed;

	/**
	 * Holds the 'J' value to be used in the random algorithm.
	 */
	private BigInteger jValue = BigInteger.valueOf(J_INITIAL_VALUE);

	/**
	 * Holds the 'K' value to be used in the random algorithm.
	 */
	private BigInteger kValue = BigInteger.ZERO;

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
	 * Retrieves a random {@link BigInteger} from the internal random seed.
	 *
	 * @param limit
	 * 		the upper limit of the random {@link BigInteger}
	 *
	 * @return the random {@link BigInteger}
	 */
	public BigInteger randomInteger(final BigInteger limit) {

		final BigInteger limitLength = BigInteger.valueOf(limit.bitLength());

		BigInteger bits = randomChunk();
		BigInteger count = limitLength.subtract(RANDOM_OVERLAP);
		while (BigInteger.ZERO.compareTo(count) <= 0) {
			final BigInteger shiftedBits = ash(bits);
			final BigInteger nextChunk = randomChunk();

			bits = shiftedBits.xor(nextChunk);
			count = count.subtract(BIT_SHIFT_AMOUNT);
		}

		return bits.remainder(limit);
	}

	/**
	 * Retrieves a random {@link BigDecimal} from the internal random seed.
	 *
	 * @param limit
	 * 		the upper limit of the random {@link BigDecimal}
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
	 * Obtains a random 'chunk', or integer value from the internal random seed.
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
	 * Performs bit shifting depending on the sign of the shift value.
	 *
	 * @param bits
	 * 		the bits to shift
	 *
	 * @return a new shifted {@link BigInteger}
	 */
	private static BigInteger ash(final BigInteger bits) {
		return (BIT_SHIFT_AMOUNT_AS_INT <= 0) ? bits.shiftRight(BIT_SHIFT_AMOUNT_AS_INT) : bits.shiftLeft(BIT_SHIFT_AMOUNT_AS_INT);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(jValue)
		                            .append(kValue)
		                            .append(seed)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final RandomStateStruct rhs = (RandomStateStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(jValue, rhs.jValue)
		                          .append(kValue, rhs.kValue)
		                          .append(seed, rhs.seed)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(jValue)
		                                                                .append(kValue)
		                                                                .append(seed)
		                                                                .toString();
	}
}
