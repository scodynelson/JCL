package jcl.compiler.real.icg;

import jcl.structs.numbers.IntegerStruct;

public class IntegerCodeGenerator {

	public static void genCodeInteger(final IntermediateCodeGenerator icg, final IntegerStruct integerStruct) {
		icg.emitter.emitLdc(integerStruct.getBigInteger().toString());
		icg.emitter.emitInvokestatic("java/math/BigInteger", "<init>", "(Ljava/lang/String;)", "V", false);
		icg.emitter.emitInvokestatic("jcl/structs/numbers/IntegerStruct", "<init>", "(Ljava/math/BigInteger;)", "V", false);
	}
}
