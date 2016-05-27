package jcl.numbers;

import jcl.LispType;
import org.apfloat.Aprational;

/**
 * Created by codynelson on 5/27/16.
 */
public class RationalStruct2Impl<A extends Aprational> extends RealStruct2Impl<A> implements RationalStruct2 {

	protected RationalStruct2Impl(final LispType type, final A ap) {
		super(type, ap);
	}

	@Override
	public Aprational ap() {
		return null;
	}

	@Override
	public IntegerStruct2 imagPart() {
		return null;
	}

	@Override
	public IntegerStruct2 numerator() {
		return null;
	}

	@Override
	public IntegerStruct2 denominator() {
		return null;
	}
}
