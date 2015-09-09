package jcl.compiler.real.icg.generator;

import java.math.BigInteger;

import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.numbers.RatioStruct;
import org.apache.commons.math3.fraction.BigFraction;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

@Component
class RatioCodeGenerator implements CodeGenerator<RatioStruct> {

	private static final String BIG_INTEGER_NAME = Type.getInternalName(BigInteger.class);

	private static final String BIG_INTEGER_INIT_DESC = GeneratorUtils.getConstructorDescription(BigInteger.class, String.class);

	private static final String RATIO_STRUCT_NAME = Type.getInternalName(RatioStruct.class);

	private static final String RATIO_STRUCT_INIT_DESC = GeneratorUtils.getConstructorDescription(RatioStruct.class, BigInteger.class, BigInteger.class);

	@Override
	public void generate(final RatioStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final BigFraction bigFraction = input.getBigFraction();

		mv.visitTypeInsn(Opcodes.NEW, BIG_INTEGER_NAME);
		mv.visitInsn(Opcodes.DUP);

		final BigInteger numerator = bigFraction.getNumerator();
		final String numeratorString = numerator.toString();
		mv.visitLdcInsn(numeratorString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				BIG_INTEGER_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				BIG_INTEGER_INIT_DESC,
				false);
		final int numeratorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, numeratorStore);

		mv.visitTypeInsn(Opcodes.NEW, BIG_INTEGER_NAME);
		mv.visitInsn(Opcodes.DUP);

		final BigInteger denominator = bigFraction.getDenominator();
		final String denominatorString = denominator.toString();
		mv.visitLdcInsn(denominatorString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				BIG_INTEGER_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				BIG_INTEGER_INIT_DESC,
				false);
		final int denominatorStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, denominatorStore);

		mv.visitTypeInsn(Opcodes.NEW, RATIO_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, numeratorStore);
		mv.visitVarInsn(Opcodes.ALOAD, denominatorStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				RATIO_STRUCT_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				RATIO_STRUCT_INIT_DESC,
				false);
	}
}
