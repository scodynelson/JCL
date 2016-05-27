package jcl.numbers;

import jcl.LispType;
import org.apfloat.Apfloat;

/**
 * Created by codynelson on 5/27/16.
 */
public class RealStruct2Impl<A extends Apfloat> extends NumberStruct2Impl<A> implements RealStruct2 {

	protected RealStruct2Impl(final LispType type, final A ap) {
		super(type, ap);
	}

	@Override
	public Apfloat ap() {
		return null;
	}

	@Override
	public boolean isLessThan(final RealStruct2 real) {
		return false;
	}

	@Override
	public boolean isGreaterThan(final RealStruct2 real) {
		return false;
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct2 real) {
		return false;
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct2 real) {
		return false;
	}

	@Override
	public boolean plusp() {
		return false;
	}

	@Override
	public boolean minusp() {
		return false;
	}

	@Override
	public RationalStruct2 rational() {
		return null;
	}

	@Override
	public FloatStruct2 floatingPoint() {
		return null;
	}

	@Override
	public FloatStruct2 floatingPoint(final FloatStruct2 prototype) {
		return null;
	}

	@Override
	public QuotientRemainderResult2 floor(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult2 ffloor(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult2 ceiling(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult2 fceiling(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult2 truncate(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult2 ftruncate(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult2 round(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public QuotientRemainderResult2 fround(final RealStruct2 divisor) {
		return null;
	}

	@Override
	public RealStruct2 realPart() {
		return null;
	}

	@Override
	public RealStruct2 conjugate() {
		return null;
	}

	@Override
	public RealStruct2 sin() {
		return null;
	}

	@Override
	public RealStruct2 cos() {
		return null;
	}

	@Override
	public RealStruct2 tan() {
		return null;
	}

	@Override
	public RealStruct2 asin() {
		return null;
	}

	@Override
	public RealStruct2 acos() {
		return null;
	}

	@Override
	public RealStruct2 atan() {
		return null;
	}

	@Override
	public RealStruct2 atan(final RealStruct2 real) {
		return null;
	}

	@Override
	public RealStruct2 sinh() {
		return null;
	}

	@Override
	public RealStruct2 cosh() {
		return null;
	}

	@Override
	public RealStruct2 tanh() {
		return null;
	}

	@Override
	public RealStruct2 asinh() {
		return null;
	}

	@Override
	public RealStruct2 acosh() {
		return null;
	}

	@Override
	public RealStruct2 atanh() {
		return null;
	}
}
