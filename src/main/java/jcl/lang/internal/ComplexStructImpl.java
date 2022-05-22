package jcl.lang.internal;

import java.util.Arrays;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.lang.BooleanStruct;
import jcl.lang.ComplexStruct;
import jcl.lang.FloatStruct;
import jcl.lang.LispStruct;
import jcl.lang.NumberStruct;
import jcl.lang.RealStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.statics.CommonLispSymbols;
import org.apfloat.Apcomplex;
import org.apfloat.ApcomplexMath;
import org.apfloat.Apfloat;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

/**
 * The {@link ComplexStruct} is the object representation of a Lisp 'complex' type.
 */
public class ComplexStructImpl extends LispStructImpl implements ComplexStruct {

	/**
	 * The real part of the complex number.
	 */
	private final RealStruct real;

	/**
	 * The imaginary part of the complex number.
	 */
	private final RealStruct imaginary;

	/**
	 * The {@link Apcomplex} representation of the complex number.
	 */
	final Apcomplex apcomplex;

	/**
	 * Public constructor.
	 *
	 * @param apcomplex
	 * 		the {@link Apcomplex} representation of the complex number
	 */
	public ComplexStructImpl(final Apcomplex apcomplex) {
		real = ApfloatUtils.toRealStruct(apcomplex.real());
		imaginary = ApfloatUtils.toRealStruct(apcomplex.imag());
		this.apcomplex = apcomplex;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		the real part of the complex number
	 * @param imaginary
	 * 		the imaginary part of the complex number
	 */
	public ComplexStructImpl(final RealStruct real, final RealStruct imaginary) {
		this.real = real;
		this.imaginary = imaginary;
		apcomplex = new Apcomplex(real.ap(), imaginary.ap());
	}

	private enum ValueType {
		RATIONAL,
		FLOAT
	}

	private static ValueType determineComplexValueType(final RealStruct... reals) {
		final boolean anyFloats
				= Arrays.stream(reals)
				        .map(Object::getClass)
				        .anyMatch(FloatStruct.class::isAssignableFrom);

		return anyFloats ? ValueType.FLOAT : ValueType.RATIONAL;
	}

	/*
	NUMBER-STRUCT
	 */

	@Override
	public Apcomplex ap() {
		return apcomplex;
	}

	@Override
	public RealStruct abs() {
		final Apfloat abs = ApcomplexMath.abs(apcomplex);
		return ApfloatUtils.toRealStruct(abs);
	}

	@Override
	public BooleanStruct zerop() {
		return BooleanStruct.toLispBoolean(Apcomplex.ZERO.equals(apcomplex));
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		final Apcomplex add = apcomplex.add(numberAp);
		return ApfloatUtils.toNumberStruct(add);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		final Apcomplex subtract = apcomplex.subtract(numberAp);
		return ApfloatUtils.toNumberStruct(subtract);
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		final Apcomplex multiply = apcomplex.multiply(numberAp);
		return ApfloatUtils.toNumberStruct(multiply);
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		final Apcomplex divide = apcomplex.divide(numberAp);
		return ApfloatUtils.toNumberStruct(divide);
	}

	@Override
	public boolean isEqualTo(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		return apcomplex.equals(numberAp);
	}

	@Override
	public boolean isNotEqualTo(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		return !apcomplex.equals(numberAp);
	}

	@Override
	public NumberStruct signum() {
		if (Apcomplex.ZERO.equals(apcomplex)) {
			return this;
		}

		final Apfloat abs = ApcomplexMath.abs(apcomplex);
		final Apcomplex signum = apcomplex.divide(abs);
		return ApfloatUtils.toNumberStruct(signum);
	}

	@Override
	public RealStruct realPart() {
		return real;
	}

	@Override
	public RealStruct imagPart() {
		return imaginary;
	}

	@Override
	public NumberStruct conjugate() {
		final Apcomplex conj = apcomplex.conj();
		return new ComplexStructImpl(conj);
	}

	@Override
	public NumberStruct negation() {
		final Apcomplex negate = apcomplex.negate();
		return new ComplexStructImpl(negate);
	}

	@Override
	public NumberStruct reciprocal() {
		final Apcomplex reciprocal = Apcomplex.ONE.divide(apcomplex);
		return ApfloatUtils.toNumberStruct(reciprocal);
	}

	@Override
	public NumberStruct exp() {
		final Apcomplex exp = ApcomplexMath.exp(apcomplex);
		return ApfloatUtils.toNumberStruct(exp);
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		final Apcomplex powerAp = power.ap();
		final Apcomplex pow = ApcomplexMath.pow(apcomplex, powerAp);
		return ApfloatUtils.toNumberStruct(pow);
	}

	@Override
	public NumberStruct log() {
		final Apcomplex log = ApcomplexMath.log(apcomplex);
		return ApfloatUtils.toNumberStruct(log);
	}

	@Override
	public NumberStruct log(final NumberStruct base) {
		final Apcomplex log = ApcomplexMath.log(apcomplex);
		return ApfloatUtils.toNumberStruct(log);
	}

	@Override
	public NumberStruct sqrt() {
		final Apcomplex sqrt = ApcomplexMath.sqrt(apcomplex);
		return ApfloatUtils.toNumberStruct(sqrt);
	}

	@Override
	public NumberStruct sin() {
		final Apcomplex sin = ApcomplexMath.sin(apcomplex);
		return ApfloatUtils.toNumberStruct(sin);
	}

	@Override
	public NumberStruct cos() {
		final Apcomplex cos = ApcomplexMath.cos(apcomplex);
		return ApfloatUtils.toNumberStruct(cos);
	}

	@Override
	public NumberStruct tan() {
		final Apcomplex tan = ApcomplexMath.tan(apcomplex);
		return ApfloatUtils.toNumberStruct(tan);
	}

	@Override
	public NumberStruct asin() {
		final Apcomplex asin = ApcomplexMath.asin(apcomplex);
		return ApfloatUtils.toNumberStruct(asin);
	}

	@Override
	public NumberStruct acos() {
		final Apcomplex acos = ApcomplexMath.acos(apcomplex);
		return ApfloatUtils.toNumberStruct(acos);
	}

	@Override
	public NumberStruct atan() {
		final Apcomplex atan = ApcomplexMath.atan(apcomplex);
		return ApfloatUtils.toNumberStruct(atan);
	}

	@Override
	public NumberStruct sinh() {
		final Apcomplex sinh = ApcomplexMath.sinh(apcomplex);
		return ApfloatUtils.toNumberStruct(sinh);
	}

	@Override
	public NumberStruct cosh() {
		final Apcomplex cosh = ApcomplexMath.cosh(apcomplex);
		return ApfloatUtils.toNumberStruct(cosh);
	}

	@Override
	public NumberStruct tanh() {
		final Apcomplex tanh = ApcomplexMath.tanh(apcomplex);
		return ApfloatUtils.toNumberStruct(tanh);
	}

	@Override
	public NumberStruct asinh() {
		final Apcomplex asinh = ApcomplexMath.asinh(apcomplex);
		return ApfloatUtils.toNumberStruct(asinh);
	}

	@Override
	public NumberStruct acosh() {
		final Apcomplex acosh = ApcomplexMath.acosh(apcomplex);
		return ApfloatUtils.toNumberStruct(acosh);
	}

	@Override
	public NumberStruct atanh() {
		final Apcomplex atanh = ApcomplexMath.atanh(apcomplex);
		return ApfloatUtils.toNumberStruct(atanh);
	}

	/*
	LISP-STRUCT
	 */

	/**
	 * Constant {@link String} containing the name for the {@link ComplexStruct} class.
	 */
	private static final String COMPLEX_NAME = Type.getInternalName(ComplexStruct.class);

	/**
	 * Constant {@link String} containing the name for the {@link ComplexStruct#toLispComplex(RealStruct, RealStruct)}
	 * method.
	 */
	private static final String COMPLEX_TO_LISP_COMPLEX_METHOD_NAME = "toLispComplex";

	/**
	 * Constant {@link String} containing the description for the {@link ComplexStruct#toLispComplex(RealStruct,
	 * RealStruct)} method.
	 */
	private static final String COMPLEX_TO_LISP_COMPLEX_METHOD_DESC
			= CodeGenerators.getMethodDescription(ComplexStruct.class, COMPLEX_TO_LISP_COMPLEX_METHOD_NAME,
			                                      RealStruct.class, RealStruct.class);

	/**
	 * {@inheritDoc}
	 * Generation method for {@link ComplexStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Emitting the {@link NumberStruct#realPart()} value.</li>
	 * <li>Emitting the {@link NumberStruct#imagPart()} value.</li>
	 * <li>Retrieving a {@link ComplexStruct} via {@link ComplexStruct#toLispComplex(RealStruct, RealStruct)} with
	 * the created real and imaginary values</li>
	 * </ol>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		real.generate(generatorState);
		imaginary.generate(generatorState);

		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   COMPLEX_NAME,
		                   COMPLEX_TO_LISP_COMPLEX_METHOD_NAME,
		                   COMPLEX_TO_LISP_COMPLEX_METHOD_DESC,
		                   true);
	}

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.COMPLEX;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.COMPLEX;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.COMPLEX) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.NUMBER) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.COMPLEX) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.NUMBER) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
