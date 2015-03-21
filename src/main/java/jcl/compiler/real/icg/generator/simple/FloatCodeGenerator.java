package jcl.compiler.real.icg.generator.simple;

import java.math.BigDecimal;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.numbers.FloatStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class FloatCodeGenerator implements CodeGenerator<FloatStruct> {

	@Override
	public void generate(final FloatStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, "java/math/BigDecimal");
		mv.visitInsn(Opcodes.DUP);

		final BigDecimal bigDecimal = input.getBigDecimal();
		final String decimalString = bigDecimal.toString();
		mv.visitLdcInsn(decimalString);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "java/math/BigDecimal", "<init>", "(Ljava/lang/String;)V", false);
		final int bigDecimalStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, bigDecimalStore);

		mv.visitTypeInsn(Opcodes.NEW, "jcl/numbers/FloatStruct");
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, bigDecimalStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/numbers/FloatStruct", "<init>", "(Ljava/math/BigDecimal;)V", false);
	}
}
