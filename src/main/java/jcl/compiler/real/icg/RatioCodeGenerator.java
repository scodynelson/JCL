package jcl.compiler.real.icg;

import jcl.numbers.RatioStruct;

public class RatioCodeGenerator implements CodeGenerator<RatioStruct> {

	public static final RatioCodeGenerator INSTANCE = new RatioCodeGenerator();

	@Override
	public void generate(final RatioStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		classBuilder.getEmitter().emitLdc(input.getBigFraction().getNumerator().toString());
		classBuilder.getEmitter().emitInvokestatic("java/math/BigInteger", "<init>", "(Ljava/lang/String;)", "V", false);
		classBuilder.getEmitter().emitLdc(input.getBigFraction().getDenominator().toString());
		classBuilder.getEmitter().emitInvokestatic("java/math/BigInteger", "<init>", "(Ljava/lang/String;)", "V", false);
		classBuilder.getEmitter().emitInvokestatic("jcl/numbers/RatioStruct", "<init>", "(Ljava/math/BigInteger;Ljava/math/BigInteger;)", "V", false);
	}
}
