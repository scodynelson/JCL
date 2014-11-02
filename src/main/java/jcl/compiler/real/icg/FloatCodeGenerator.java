package jcl.compiler.real.icg;

import jcl.structs.numbers.FloatStruct;

public class FloatCodeGenerator {

	public static void genCodeFloat(final IntermediateCodeGenerator icg, final FloatStruct floatStruct) {
		icg.emitter.emitLdc(floatStruct.getBigDecimal().toString());
		icg.emitter.emitInvokestatic("java/math/BigDecimal", "<init>", "(Ljava/lang/String;)", "V", false);
		icg.emitter.emitInvokestatic("jcl/structs/numbers/FloatStruct", "<init>", "(Ljava/math/BigDecimal;)", "V", false);
	}
}
