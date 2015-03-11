package jcl.compiler.real.icg;

import jcl.lists.ListStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RatioStruct;
import jcl.numbers.RealStruct;

public class ComplexCodeGenerator implements CodeGenerator<ListStruct> {

	public static final ComplexCodeGenerator INSTANCE = new ComplexCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		// TODO: we NEED to do Complex numbers better!!
		// TODO: now this is hacked up. we probably shouldn't even have this generator to begin with...
		final RealStruct real = (RealStruct) input.getRest().getFirst();
		final RealStruct imaginary = (RealStruct) input.getRest().getRest().getFirst();

		if (real instanceof IntegerStruct) {
			IntegerCodeGenerator.INSTANCE.generate((IntegerStruct) real, codeGenerator, classBuilder);
		} else if (real instanceof FloatStruct) {
			FloatCodeGenerator.INSTANCE.generate((FloatStruct) real, codeGenerator, classBuilder);
		} else if (real instanceof RatioStruct) {
			RatioCodeGenerator.INSTANCE.generate((RatioStruct) real, codeGenerator, classBuilder);
		} else {
			throw new RuntimeException("Only reals are valid for the Complex 'real' part.");
		}

		if (imaginary instanceof IntegerStruct) {
			IntegerCodeGenerator.INSTANCE.generate((IntegerStruct) imaginary, codeGenerator, classBuilder);
		} else if (imaginary instanceof FloatStruct) {
			FloatCodeGenerator.INSTANCE.generate((FloatStruct) imaginary, codeGenerator, classBuilder);
		} else if (imaginary instanceof RatioStruct) {
			RatioCodeGenerator.INSTANCE.generate((RatioStruct) imaginary, codeGenerator, classBuilder);
		} else {
			throw new RuntimeException("Only reals are valid for the Complex 'real' part.");
		}

		classBuilder.getEmitter().emitInvokestatic("jcl/numbers/ComplexStruct", "getInstance", "(Ljcl/numbers/RealStruct;Ljcl/numbers/RealStruct;)", "Ljcl/numbers/ComplexStruc;", false);
	}
}
