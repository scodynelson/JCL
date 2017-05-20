package jcl.lang.internal;

import jcl.lang.IntegerStruct;
import jcl.lang.NumberStruct;
import jcl.lang.RationalStruct;
import jcl.lang.RealStruct;
import lombok.experimental.UtilityClass;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;
import org.apfloat.Apint;
import org.apfloat.Aprational;

@UtilityClass
public final class ApfloatUtils {

	public static NumberStruct toNumberStruct(final Apcomplex apcomplex) {
		final Apfloat real = apcomplex.real();
		final Apfloat imag = apcomplex.imag();

		if (Apcomplex.ZERO.compareTo(imag) == 0) {
			return toRealStruct(real);
		}
		return new ComplexStructImpl(apcomplex);
	}

	public static RealStruct toRealStruct(final Apfloat apfloat) {
		if (apfloat instanceof Aprational) {
			return toRationalStruct((Aprational) apfloat);
		}
		// TODO: default to DoubleFloat???
		return new DoubleFloatStructImpl(apfloat.doubleValue());
	}

	public static RationalStruct toRationalStruct(final Aprational aprational) {
		if (aprational instanceof Apint) {
			return toIntegerStruct((Apint) aprational);
		}
		final Apint numeratorAp = aprational.numerator();
		final IntegerStruct numerator = toIntegerStruct(numeratorAp);

		final Apint denominatorAp = aprational.denominator();
		final IntegerStruct denominator = toIntegerStruct(denominatorAp);

		return RationalStruct.toLispRational(numerator, denominator);
	}

	public static IntegerStruct toIntegerStruct(final Apint apint) {
		return IntegerStruct.toLispInteger(apint.toBigInteger());
	}
}
