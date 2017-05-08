package jcl.lang.internal;

import jcl.lang.ComplexStruct;
import jcl.lang.NumberStruct;
import jcl.lang.RealStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.type.ComplexType;
import lombok.EqualsAndHashCode;
import org.apfloat.Apcomplex;

@EqualsAndHashCode(callSuper = true)
public class ComplexStructImpl extends BuiltInClassStruct implements ComplexStruct {

	private final RealStruct real;
	private final RealStruct imaginary;

	public ComplexStructImpl(final RealStruct real, final RealStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	@Override
	public ValueType getValueType() {
		// TODO
		return null;
	}

	/*
	NUMBER-STRUCT
	 */

	@Override
	public Apcomplex ap() {
		// TODO
		return null;
	}

	@Override
	public RealStruct abs() {
		// TODO
		return null;
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
	public NumberStruct signum() {
		// TODO
		return null;
	}

	@Override
	public RealStruct realPart() {
		// TODO
		return null;
	}

	@Override
	public RealStruct imagPart() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct conjugate() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct negation() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct reciprocal() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct exp() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		// TODO
		return null;
	}

	@Override
	public NumberStruct log() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct log(final NumberStruct base) {
		// TODO
		return null;
	}

	@Override
	public NumberStruct sqrt() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct sin() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct cos() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct tan() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct asin() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct acos() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct atan() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct sinh() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct cosh() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct tanh() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct asinh() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct acosh() {
		// TODO
		return null;
	}

	@Override
	public NumberStruct atanh() {
		// TODO
		return null;
	}
}
