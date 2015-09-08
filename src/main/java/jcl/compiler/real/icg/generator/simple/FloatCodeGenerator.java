package jcl.compiler.real.icg.generator.simple;

import java.math.BigDecimal;

import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.compiler.real.icg.generator.GeneratorUtils;
import jcl.numbers.FloatStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

@Component
public class FloatCodeGenerator implements CodeGenerator<FloatStruct> {

	private static final String BIG_DECIMAL_NAME = Type.getInternalName(BigDecimal.class);

	private static final String BIG_DECIMAL_INIT_DESC = GeneratorUtils.getConstructorDescription(BigDecimal.class, String.class);

	private static final String FLOAT_STRUCT_NAME = Type.getInternalName(FloatStruct.class);

	private static final String FLOAT_STRUCT_INIT_DESC = GeneratorUtils.getConstructorDescription(FloatStruct.class, BigDecimal.class);

	@Override
	public void generate(final FloatStruct input, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, BIG_DECIMAL_NAME);
		mv.visitInsn(Opcodes.DUP);

		final BigDecimal bigDecimal = input.getBigDecimal();
		final String decimalString = bigDecimal.toString();
		mv.visitLdcInsn(decimalString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				BIG_DECIMAL_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				BIG_DECIMAL_INIT_DESC,
				false);
		final int bigDecimalStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, bigDecimalStore);

		mv.visitTypeInsn(Opcodes.NEW, FLOAT_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, bigDecimalStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				FLOAT_STRUCT_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				FLOAT_STRUCT_INIT_DESC,
				false);
	}
}
