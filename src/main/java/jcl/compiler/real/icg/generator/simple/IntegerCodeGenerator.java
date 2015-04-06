package jcl.compiler.real.icg.generator.simple;

import java.math.BigInteger;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.GeneratorUtils;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.numbers.IntegerStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.stereotype.Component;

@Component
public class IntegerCodeGenerator implements CodeGenerator<IntegerStruct> {

	private static final String BIG_INTEGER_NAME = Type.getInternalName(BigInteger.class);

	private static final String BIG_INTEGER_INIT_DESC = GeneratorUtils.getConstructorDescription(BigInteger.class, String.class);

	private static final String INTEGER_STRUCT_NAME = Type.getInternalName(IntegerStruct.class);

	private static final String INTEGER_STRUCT_INIT_DESC = GeneratorUtils.getConstructorDescription(IntegerStruct.class, BigInteger.class);

	@Override
	public void generate(final IntegerStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, BIG_INTEGER_NAME);
		mv.visitInsn(Opcodes.DUP);

		final BigInteger bigInteger = input.getBigInteger();
		final String integerString = bigInteger.toString();
		mv.visitLdcInsn(integerString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, BIG_INTEGER_NAME, GenerationConstants.INIT_METHOD_NAME, BIG_INTEGER_INIT_DESC, false);
		final int bigIntegerStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, bigIntegerStore);

		mv.visitTypeInsn(Opcodes.NEW, INTEGER_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, bigIntegerStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, INTEGER_STRUCT_NAME, GenerationConstants.INIT_METHOD_NAME, INTEGER_STRUCT_INIT_DESC, false);
	}
}
