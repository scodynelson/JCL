package jcl.compiler.real.icg;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.FloatElement;
import jcl.compiler.real.element.IntegerElement;
import jcl.compiler.real.element.RatioElement;
import jcl.compiler.real.element.RealElement;

public class ComplexCodeGenerator implements CodeGenerator<ConsElement> {

	public static final ComplexCodeGenerator INSTANCE = new ComplexCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {
		// TODO: we NEED to do Complex numbers better!!
		// TODO: now this is hacked up. we probably shouldn't even have this generator to begin with...
		final RealElement real = (RealElement) input.getElements().get(1);
		final RealElement imaginary = (RealElement) input.getElements().get(2);

		if (real instanceof IntegerElement) {
			IntegerCodeGenerator.INSTANCE.generate((IntegerElement) real, codeGenerator);
		} else if (real instanceof FloatElement) {
			FloatCodeGenerator.INSTANCE.generate((FloatElement) real, codeGenerator);
		} else if (real instanceof RatioElement) {
			RatioCodeGenerator.INSTANCE.generate((RatioElement) real, codeGenerator);
		} else {
			throw new RuntimeException("Only reals are valid for the Complex 'real' part.");
		}

		if (imaginary instanceof IntegerElement) {
			IntegerCodeGenerator.INSTANCE.generate((IntegerElement) imaginary, codeGenerator);
		} else if (imaginary instanceof FloatElement) {
			FloatCodeGenerator.INSTANCE.generate((FloatElement) imaginary, codeGenerator);
		} else if (imaginary instanceof RatioElement) {
			RatioCodeGenerator.INSTANCE.generate((RatioElement) imaginary, codeGenerator);
		} else {
			throw new RuntimeException("Only reals are valid for the Complex 'real' part.");
		}

		codeGenerator.emitter.emitInvokestatic("jcl/numbers/ComplexStruct", "getInstance", "(Ljcl/numbers/RealStruct;Ljcl/numbers/RealStruct;)", "Ljcl/numbers/ComplexStruc;", false);
	}
}
