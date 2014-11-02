package jcl.compiler.real.icg;

import jcl.structs.numbers.RatioStruct;

public class RatioCodeGenerator {

	public static void genCodeRatio(final IntermediateCodeGenerator icg, final RatioStruct ratioStruct) {
		icg.emitter.emitLdc(ratioStruct.getBigFraction().getNumerator().toString());
		icg.emitter.emitInvokestatic("java/math/BigInteger", "<init>", "(Ljava/lang/String;)", "V", false);
		icg.emitter.emitLdc(ratioStruct.getBigFraction().getDenominator().toString());
		icg.emitter.emitInvokestatic("java/math/BigInteger", "<init>", "(Ljava/lang/String;)", "V", false);
		icg.emitter.emitInvokestatic("jcl/structs/numbers/RatioStruct", "<init>", "(Ljava/math/BigInteger;Ljava/math/BigInteger;)", "V", false);
	}
}
