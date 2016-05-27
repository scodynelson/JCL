package jcl.numbers;

import jcl.LispType;
import org.apfloat.Apcomplex;

/**
 * Created by codynelson on 5/27/16.
 */
public class NumberStruct2Impl<A extends Apcomplex> extends InternalNumberStruct<A> implements NumberStruct2 {

	protected NumberStruct2Impl(final LispType type, final A ap) {
		super(type, ap);
	}

	@Override
	public Apcomplex ap() {
		return null;
	}

	@Override
	public RealStruct2 abs() {
		return null;
	}

	@Override
	public boolean zerop() {
		return false;
	}

	@Override
	public NumberStruct2 add(final NumberStruct2 number) {
		return null;
	}

	@Override
	public NumberStruct2 subtract(final NumberStruct2 number) {
		return null;
	}

	@Override
	public NumberStruct2 multiply(final NumberStruct2 number) {
		return null;
	}

	@Override
	public NumberStruct2 divide(final NumberStruct2 number) {
		return null;
	}

	@Override
	public boolean isEqualTo(final NumberStruct2 number) {
		return false;
	}

	@Override
	public NumberStruct2 signum() {
		return null;
	}

	@Override
	public NumberStruct2 realPart() {
		return null;
	}

	@Override
	public NumberStruct2 imagPart() {
		return null;
	}

	@Override
	public NumberStruct2 conjugate() {
		return null;
	}

	@Override
	public NumberStruct2 negation() {
		return null;
	}

	@Override
	public NumberStruct2 reciprocal() {
		return null;
	}

	@Override
	public NumberStruct2 exp() {
		return null;
	}

	@Override
	public NumberStruct2 expt(final NumberStruct2 power) {
		return null;
	}

	@Override
	public NumberStruct2 log() {
		return null;
	}

	@Override
	public NumberStruct2 sqrt() {
		return null;
	}

	@Override
	public NumberStruct2 sin() {
		return null;
	}

	@Override
	public NumberStruct2 cos() {
		return null;
	}

	@Override
	public NumberStruct2 tan() {
		return null;
	}

	@Override
	public NumberStruct2 asin() {
		return null;
	}

	@Override
	public NumberStruct2 acos() {
		return null;
	}

	@Override
	public NumberStruct2 atan() {
		return null;
	}

	@Override
	public NumberStruct2 sinh() {
		return null;
	}

	@Override
	public NumberStruct2 cosh() {
		return null;
	}

	@Override
	public NumberStruct2 tanh() {
		return null;
	}

	@Override
	public NumberStruct2 asinh() {
		return null;
	}

	@Override
	public NumberStruct2 acosh() {
		return null;
	}

	@Override
	public NumberStruct2 atanh() {
		return null;
	}
}
