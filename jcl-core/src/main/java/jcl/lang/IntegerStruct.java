package jcl.lang;

import java.math.BigInteger;
import java.util.List;

import jcl.lang.internal.BignumStructImpl;
import jcl.lang.internal.FixnumStructImpl;
import jcl.lang.internal.LongnumStructImpl;
import jcl.lang.statics.CommonLispSymbols;
import org.apfloat.Apint;

/**
 * The {@link IntegerStruct} is the object representation of a Lisp 'integer' type.
 */
public interface IntegerStruct extends RationalStruct {

	/**
	 * {@link IntegerStruct} constant representing 0.
	 */
	FixnumStruct ZERO = toLispInteger(0);

	/**
	 * {@link IntegerStruct} constant representing 1.
	 */
	FixnumStruct ONE = toLispInteger(1);

	/**
	 * {@link IntegerStruct} constant representing 2.
	 */
	FixnumStruct TWO = toLispInteger(2);

	/**
	 * {@link IntegerStruct} constant representing 3.
	 */
	FixnumStruct THREE = toLispInteger(3);

	/**
	 * {@link IntegerStruct} constant representing 4.
	 */
	FixnumStruct FOUR = toLispInteger(4);

	/**
	 * {@link IntegerStruct} constant representing 5.
	 */
	FixnumStruct FIVE = toLispInteger(5);

	/**
	 * {@link IntegerStruct} constant representing 6.
	 */
	FixnumStruct SIX = toLispInteger(6);

	/**
	 * {@link IntegerStruct} constant representing 7.
	 */
	FixnumStruct SEVEN = toLispInteger(7);

	/**
	 * {@link IntegerStruct} constant representing 8.
	 */
	FixnumStruct EIGHT = toLispInteger(8);

	/**
	 * {@link IntegerStruct} constant representing 9.
	 */
	FixnumStruct NINE = toLispInteger(9);

	/**
	 * {@link IntegerStruct} constant representing 10.
	 */
	FixnumStruct TEN = toLispInteger(10);

	/**
	 * {@link IntegerStruct} constant representing 11.
	 */
	FixnumStruct ELEVEN = toLispInteger(11);

	/**
	 * {@link IntegerStruct} constant representing 12.
	 */
	FixnumStruct TWELVE = toLispInteger(12);

	/**
	 * {@link IntegerStruct} constant representing 13.
	 */
	FixnumStruct THIRTEEN = toLispInteger(13);

	/**
	 * {@link IntegerStruct} constant representing 14.
	 */
	FixnumStruct FOURTEEN = toLispInteger(14);

	/**
	 * {@link IntegerStruct} constant representing 15.
	 */
	FixnumStruct FIFTEEN = toLispInteger(15);

	/**
	 * {@link IntegerStruct} constant representing 16.
	 */
	FixnumStruct SIXTEEN = toLispInteger(16);

	/**
	 * {@link IntegerStruct} constant representing -1.
	 */
	FixnumStruct MINUS_ONE = toLispInteger(-1);

	/**
	 * Returns the greatest common divisor of the provided IntegerStructs. If the number of IntegerStructs provided is
	 * 0, {@link #ZERO} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used to determine the greatest common divisor
	 *
	 * @return the greatest common divisor of the provided IntegerStructs
	 */
	static IntegerStruct gcd(final List<IntegerStruct> integers) {
		return integers.stream().reduce(ZERO, IntegerStruct::gcd);
	}

	/**
	 * Returns the greatest common divisor between this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct in comparison to this IntegerStruct to determine the greatest common divisor
	 *
	 * @return the greatest common divisor between this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct gcd(final IntegerStruct integer);

	/**
	 * Returns the least common multiple of the provided IntegerStructs. If the number of IntegerStructs provided is 0,
	 * {@link #ONE} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used to determine the least common multiple
	 *
	 * @return the least common multiple of the provided IntegerStructs
	 */
	static IntegerStruct lcm(final List<IntegerStruct> integers) {
		return integers.stream().reduce(ONE, IntegerStruct::lcm);
	}

	/**
	 * Returns the least common multiple between this IntegerStruct and the provided IntegerStruct. If this or the
	 * provided IntegerStruct are '0', the result is {@link #ZERO}.
	 *
	 * @param integer
	 * 		the IntegerStruct in comparison to this IntegerStruct to determine the least common multiple
	 *
	 * @return the least common multiple between this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct lcm(final IntegerStruct integer);

	/**
	 * Performs the arithmetic shift operation on the binary representation of this IntegerStruct, shifting the bits
	 * left or right by the provided {@code count} IntegerStruct based on its sign. If the {@code count} value is '0',
	 * the result is {@code this}.
	 *
	 * @param count
	 * 		the bit positions to shift this IntegerStruct left or right
	 *
	 * @return the arithmetic shift operation on the binary representation of this IntegerStruct
	 */
	IntegerStruct ash(final IntegerStruct count);

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct logAnd(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'and' of the provided IntegerStructs. If the number of IntegerStructs provided is
	 * 0, {@link #MINUS_ONE} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of the provided IntegerStructs
	 */
	static IntegerStruct logAnd(final List<IntegerStruct> integers) {
		return integers.stream().reduce(MINUS_ONE, IntegerStruct::logAnd);
	}

	/**
	 * Returns the bit-wise logical 'and' of the compliment of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct logAndC1(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'and' of this IntegerStruct and the compliment of provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'and' of this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct logAndC2(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'equivalence', or 'exclusive-nor' of the provided IntegerStructs. If the number
	 * of IntegerStructs provided is 0, {@link #MINUS_ONE} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'equivalence', or 'exclusive-nor' of the provided IntegerStructs
	 */
	static IntegerStruct logEqv(final List<IntegerStruct> integers) {
		return integers.stream().reduce(MINUS_ONE, IntegerStruct::logEqv);
	}

	/**
	 * Returns the bit-wise logical 'equivalence', or 'exclusive-nor' of this IntegerStruct and the provided
	 * IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'equivalence', or 'exclusive-nor' of this IntegerStruct and the provided
	 * IntegerStruct
	 */
	IntegerStruct logEqv(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'inclusive-or' of the provided IntegerStructs. If the number of IntegerStructs
	 * provided is 0, {@link #ZERO} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of the provided IntegerStructs
	 */
	static IntegerStruct logIor(final List<IntegerStruct> integers) {
		return integers.stream().reduce(ZERO, IntegerStruct::logIor);
	}

	/**
	 * Returns the bit-wise logical 'inclusive-or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct logIor(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'nand' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'nand' of this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct logNand(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'nor' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'nor' of this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct logNor(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'not' of this IntegerStruct.
	 *
	 * @return the bit-wise logical 'not' of this IntegerStruct
	 */
	IntegerStruct logNot();

	/**
	 * Returns the bit-wise logical 'inclusive-or' of the compliment of this IntegerStruct and the provided
	 * IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of the compliment of this IntegerStruct and the provided
	 * IntegerStruct
	 */
	IntegerStruct logOrC1(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'inclusive-or' of this IntegerStruct and the compliment of provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'inclusive-or' of this IntegerStruct and the compliment of provided IntegerStruct
	 */
	IntegerStruct logOrC2(final IntegerStruct integer);

	/**
	 * Returns the bit-wise logical 'exclusive-or' of the provided IntegerStructs. If the number of IntegerStructs
	 * provided is 0, {@link #ZERO} is returned.
	 *
	 * @param integers
	 * 		the IntegerStructs used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'exclusive-or' of the provided IntegerStructs
	 */
	static IntegerStruct logXor(final List<IntegerStruct> integers) {
		return integers.stream().reduce(ZERO, IntegerStruct::logXor);
	}

	/**
	 * Returns the bit-wise logical 'exclusive-or' of this IntegerStruct and the provided IntegerStruct.
	 *
	 * @param integer
	 * 		the IntegerStruct used in performing the bit-wise logical operation
	 *
	 * @return the bit-wise logical 'exclusive-or' of this IntegerStruct and the provided IntegerStruct
	 */
	IntegerStruct logXor(final IntegerStruct integer);

	/**
	 * Returns true if the bit in this IntegerStruct whose index is {@code index} is a one-bit; otherwise, returns
	 * false.
	 *
	 * @param index
	 * 		the index value to test this IntegerStruct for a one-bit
	 *
	 * @return true if the bit in this IntegerStruct whose index is {@code index} is a one-bit; otherwise, false
	 */
	BooleanStruct logBitP(final IntegerStruct index);

	/**
	 * Computes and returns the number of bits in the two's-complement binary representation of this IntegerStruct that
	 * are 'on' or 'set'. If this IntegerStruct is negative, the 0 bits are counted; otherwise, the 1 bits are counted.
	 *
	 * @return Computes and returns the number of bits in the two's-complement binary representation of this
	 * IntegerStruct that are 'on' or 'set'
	 */
	IntegerStruct logCount();

	/**
	 * Returns true if any of the bits designated by the 1's in this IntegerStruct are 1 in the provided
	 * IntegerStruct; otherwise, returns false.
	 *
	 * @param integer
	 * 		the IntegerStruct used in the test comparison to this IntegerStruct
	 *
	 * @return true if any of the bits designated by the 1's in this IntegerStruct are 1 in the provided
	 * IntegerStruct; otherwise, false.
	 */
	BooleanStruct logTest(final IntegerStruct integer);

	/**
	 * Returns the number of bits needed to represent this IntegerStruct in binary two's-complement format.
	 *
	 * @return the number of bits needed to represent this IntegerStruct in binary two's-complement format
	 */
	IntegerStruct integerLength();

	/**
	 * Returns true if this IntegerStruct is even (divisible by two); otherwise, returns false.
	 *
	 * @return true if this IntegerStruct is even (divisible by two); otherwise, false
	 */
	BooleanStruct evenp();

	/**
	 * Returns true if this IntegerStruct is odd (not divisible by two); otherwise, returns false.
	 *
	 * @return true if this IntegerStruct is odd (not divisible by two); otherwise, false
	 */
	BooleanStruct oddp();

	/**
	 * Returns the greatest IntegerStruct less than or equal to this IntegerStructs exact positive square root.
	 *
	 * @return the greatest IntegerStruct less than or equal to this IntegerStructs exact positive square root
	 */
	NumberStruct isqrt();

	/**
	 * Returns the {@literal int} representation of the IntegerStruct.
	 *
	 * @return a {@literal int} representation of the IntegerStruct
	 */
	int toJavaInt();

	/**
	 * Returns the {@link Integer} representation of the IntegerStruct.
	 *
	 * @return a {@link Integer} representation of the IntegerStruct
	 */
	Integer toJavaInteger();

	/**
	 * Returns the {@literal long} representation of the IntegerStruct.
	 *
	 * @return a {@literal long} representation of the IntegerStruct
	 */
	long toJavaPLong();

	/**
	 * Returns the {@link Long} representation of the IntegerStruct.
	 *
	 * @return a {@link Long} representation of the IntegerStruct
	 */
	Long toJavaLong();

	/**
	 * Returns the {@link BigInteger} representation of the IntegerStruct.
	 *
	 * @return a {@link BigInteger} representation of the IntegerStruct
	 */
	BigInteger toJavaBigInteger();

	static FixnumStruct toLispInteger(final int value) {
		return new FixnumStructImpl(value);
	}

	static FixnumStruct toLispInteger(final Integer value) {
		return new FixnumStructImpl(value);
	}

	@SuppressWarnings("NumericCastThatLosesPrecision")
	static IntegerStruct toLispInteger(final long value) {
		if ((int) value == value) {
			return new FixnumStructImpl((int) value);
		}
		return new LongnumStructImpl(value);
	}

	static IntegerStruct toLispInteger(final Long value) {
		if (value.intValue() == value) {
			return new FixnumStructImpl(value.intValue());
		}
		return new LongnumStructImpl(value);
	}

	static IntegerStruct toLispInteger(final BigInteger value) {
		if (value.bitLength() <= (Integer.SIZE - 1)) {
			return new FixnumStructImpl(value.intValue());
		}
		if (value.bitLength() <= (Long.SIZE - 1)) {
			return new LongnumStructImpl(value.longValue());
		}
		return new BignumStructImpl(value);
	}

	/*
	RATIONAL-STRUCT
	 */

	@Override
	default IntegerStruct numerator() {
		return this;
	}

	@Override
	default IntegerStruct denominator() {
		return ONE;
	}

	/*
	REAL-STRUCT
	 */

	@Override
	default IntegerStruct rational() {
		return this;
	}

	/*
	NUMBER-STRUCT
	 */

	@Override
	Apint ap();

	@Override
	IntegerStruct abs();

	@Override
	default IntegerStruct realPart() {
		return this;
	}

	@Override
	default IntegerStruct conjugate() {
		return this;
	}

	@Override
	IntegerStruct negation();

	/*
	LISP-STRUCT
	 */

	@Override
	default boolean eql(final LispStruct object) {
		return eq(object) ||
				((object instanceof IntegerStruct)
						&& ((IntegerStruct) object).ap().equals(ap()));
	}

	LispStruct UNSIGNED_BYTE_8 = ListStruct.toLispList(CommonLispSymbols.UNSIGNED_BYTE, new FixnumStructImpl(8));
	LispStruct UNSIGNED_BYTE_16 = ListStruct.toLispList(CommonLispSymbols.UNSIGNED_BYTE, new FixnumStructImpl(16));
	LispStruct UNSIGNED_BYTE_32 = ListStruct.toLispList(CommonLispSymbols.UNSIGNED_BYTE, new FixnumStructImpl(32));
	LispStruct UNSIGNED_BYTE_32_MAX_VALUE = new BignumStructImpl(BigInteger.valueOf(4294967296L));
}
