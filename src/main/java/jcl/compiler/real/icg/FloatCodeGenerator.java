package jcl.compiler.real.icg;

import jcl.structs.numbers.FloatStruct;

public class FloatCodeGenerator implements CodeGenerator<FloatStruct> {

	public static final FloatCodeGenerator INSTANCE = new FloatCodeGenerator();

	@Override
	public void generate(final FloatStruct input, final IntermediateCodeGenerator codeGenerator) {
		codeGenerator.emitter.emitLdc(input.getBigDecimal().toString());
		codeGenerator.emitter.emitInvokestatic("java/math/BigDecimal", "<init>", "(Ljava/lang/String;)", "V", false);
		codeGenerator.emitter.emitInvokestatic("jcl/structs/numbers/FloatStruct", "<init>", "(Ljava/math/BigDecimal;)", "V", false);
	}
}
