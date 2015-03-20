package jcl.compiler.real.icg.generator.simple;

import java.math.BigDecimal;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.numbers.FloatStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class FloatCodeGenerator implements CodeGenerator<FloatStruct> {

	@Override
	public void generate(final FloatStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final ClassWriter cw = currentClass.getClassWriter();
		MethodVisitor mv = currentClass.getMethodVisitor();

		mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "floatGen", "()Ljava/lang/Object;", null, null);
		mv.visitCode();
		// TODO: don't know if we need the above 2 lines...

		final Label getBigDecimal = new Label();
		mv.visitLabel(getBigDecimal);
//		mv.visitLineNumber(109, getBigDecimal);
		mv.visitTypeInsn(Opcodes.NEW, "java/math/BigDecimal");
		mv.visitInsn(Opcodes.DUP);

		final BigDecimal bigDecimal = input.getBigDecimal();
		final String decimalString = bigDecimal.toString();
		mv.visitLdcInsn(decimalString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/math/BigDecimal", "<init>", "(Ljava/lang/String;)V", false);
		mv.visitVarInsn(Opcodes.ASTORE, 1);

		final Label getFloatStruct = new Label();
		mv.visitLabel(getFloatStruct);
//		mv.visitLineNumber(110, getFloatStruct);
		mv.visitTypeInsn(Opcodes.NEW, "jcl/numbers/FloatStruct");
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, 1);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/numbers/FloatStruct", "<init>", "(Ljava/math/BigDecimal;)V", false);

		// TODO: don't know if the next line is necessary. we might want to remain in the same method...
		mv.visitInsn(Opcodes.ARETURN);

		final Label localVariables = new Label();
		mv.visitLabel(localVariables);
		mv.visitLocalVariable("bigDecimal", "Ljava/math/BigDecimal;", null, getFloatStruct, localVariables, 1);

		// TODO: don't know if we need the next 2 lines
		mv.visitMaxs(3, 1);
		mv.visitEnd();
	}
}
