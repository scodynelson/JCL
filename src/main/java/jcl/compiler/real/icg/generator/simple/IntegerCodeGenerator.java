package jcl.compiler.real.icg.generator.simple;

import java.math.BigInteger;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.numbers.IntegerStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class IntegerCodeGenerator implements CodeGenerator<IntegerStruct> {

	@Override
	public void generate(final IntegerStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final ClassWriter cw = currentClass.getClassWriter();
		MethodVisitor mv = currentClass.getMethodVisitor();

		mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "integerGen", "()Ljava/lang/Object;", null, null);
		mv.visitCode();
		// TODO: don't know if we need the above 2 lines...

		Label getBigInteger = new Label();
		mv.visitLabel(getBigInteger);
//		mv.visitLineNumber(114, getBigInteger);
		mv.visitTypeInsn(Opcodes.NEW, "java/math/BigInteger");
		mv.visitInsn(Opcodes.DUP);

		final BigInteger bigInteger = input.getBigInteger();
		final String integerString = bigInteger.toString();
		mv.visitLdcInsn(integerString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/math/BigInteger", "<init>", "(Ljava/lang/String;)V", false);
		mv.visitVarInsn(Opcodes.ASTORE, 1);

		final Label getIntegerStruct = new Label();
		mv.visitLabel(getIntegerStruct);
//		mv.visitLineNumber(115, getIntegerStruct);
		mv.visitTypeInsn(Opcodes.NEW, "jcl/numbers/IntegerStruct");
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, 1);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/numbers/IntegerStruct", "<init>", "(Ljava/math/BigInteger;)V", false);

		// TODO: don't know if the next line is necessary. we might want to remain in the same method...
		mv.visitInsn(Opcodes.ARETURN);

		final Label localVariables = new Label();
		mv.visitLabel(localVariables);
		mv.visitLocalVariable("bigInteger", "Ljava/math/BigInteger;", null, getIntegerStruct, localVariables, 1);

		// TODO: don't know if we need the next 2 lines
		mv.visitMaxs(3, 1);
		mv.visitEnd();
	}
}
