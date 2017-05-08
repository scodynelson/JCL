package jcl.lang.internal;

import jcl.lang.FloatStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.NumberStruct;
import jcl.lang.RatioStruct;
import jcl.lang.RationalStruct;
import jcl.lang.RealStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.number.QuotientRemainder;
import jcl.type.RatioType;
import lombok.EqualsAndHashCode;
import org.apache.commons.math3.fraction.BigFraction;
import org.apfloat.Aprational;

@EqualsAndHashCode(callSuper = true)
public class RatioStructImpl extends BuiltInClassStruct implements RatioStruct {

	private final IntegerStruct numerator;
	private final IntegerStruct denominator;
	private final BigFraction value;

	public RatioStructImpl(final IntegerStruct numerator, final IntegerStruct denominator) {
		super(RatioType.INSTANCE, null, null);
		this.numerator = numerator;
		this.denominator = denominator;
		value = new BigFraction(numerator.bigIntegerValue(), denominator.bigIntegerValue());
	}

	public RatioStructImpl(final BigFraction value) {
		super(RatioType.INSTANCE, null, null);
		numerator = IntegerStruct.toLispInteger(value.getNumerator());
		denominator = IntegerStruct.toLispInteger(value.getDenominator());
		this.value = value;
	}

	@Override
	public BigFraction toBigFraction() {
		return value;
	}

	/*
	RATIONAL-STRUCT
	 */

	@Override
	public IntegerStruct numerator() {
		return numerator;
	}

	@Override
	public IntegerStruct denominator() {
		return denominator;
	}

	/*
	REAL-STRUCT
	 */

	@Override
	public boolean isLessThan(final RealStruct real) {
		// TODO
		return false;
	}

	@Override
	public boolean isGreaterThan(final RealStruct real) {
		// TODO
		return false;
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct real) {
		// TODO
		return false;
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct real) {
		// TODO
		return false;
	}

	@Override
	public boolean plusp() {
		// TODO
		return false;
	}

	@Override
	public boolean minusp() {
		// TODO
		return false;
	}

	@Override
	public FloatStruct floatingPoint() {
		// TODO
		return null;
	}

	@Override
	public FloatStruct floatingPoint(final FloatStruct prototype) {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder floor() {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder floor(final RealStruct divisor) {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder ffloor() {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder ffloor(final RealStruct divisor) {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder ceiling() {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder ceiling(final RealStruct divisor) {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder fceiling() {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder fceiling(final RealStruct divisor) {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder truncate() {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder truncate(final RealStruct divisor) {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder ftruncate() {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder ftruncate(final RealStruct divisor) {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder round() {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder round(final RealStruct divisor) {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder fround() {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder fround(final RealStruct divisor) {
		// TODO
		return null;
	}

	@Override
	public RealStruct atan(final RealStruct real) {
		// TODO
		return null;
	}

	/*
	NUMBER-STRUCT
	 */

	@Override
	public Aprational ap() {
		// TODO
		return null;
	}

	@Override
	public RatioStruct abs() {
		final BigFraction abs = value.abs();
		return new RatioStructImpl(abs);
	}

	@Override
	public boolean zerop() {
		// TODO
		return false;
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		// TODO
		return null;
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		// TODO
		return null;
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		// TODO
		return null;
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		// TODO
		return null;
	}

	@Override
	public boolean isEqualTo(final NumberStruct number) {
		// TODO
		return false;
	}

	@Override
	public boolean isNotEqualTo(final NumberStruct number) {
		// TODO
		return false;
	}

	@Override
	public IntegerStruct signum() {
		// TODO
		return null;
	}

	@Override
	public RatioStruct negation() {
		// TODO
		return null;
	}

	@Override
	public RationalStruct reciprocal() {
		// TODO
		return null;
	}

	@Override
	public RealStruct exp() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		// TODO
		return null;
	}

	@Override
	public RealStruct log() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct log(final NumberStruct base) {
		// TODO
		return null;
	}

	@Override
	public RealStruct sqrt() {
		// TODO
		return null;
	}

	@Override
	public RealStruct sin() {
		// TODO
		return null;
	}

	@Override
	public RealStruct cos() {
		// TODO
		return null;
	}

	@Override
	public RealStruct tan() {
		// TODO
		return null;
	}

	@Override
	public RealStruct asin() {
		// TODO
		return null;
	}

	@Override
	public RealStruct acos() {
		// TODO
		return null;
	}

	@Override
	public RealStruct atan() {
		// TODO
		return null;
	}

	@Override
	public RealStruct sinh() {
		// TODO
		return null;
	}

	@Override
	public RealStruct cosh() {
		// TODO
		return null;
	}

	@Override
	public RealStruct tanh() {
		// TODO
		return null;
	}

	@Override
	public RealStruct asinh() {
		// TODO
		return null;
	}

	@Override
	public RealStruct acosh() {
		// TODO
		return null;
	}

	@Override
	public RealStruct atanh() {
		// TODO
		return null;
	}
}
