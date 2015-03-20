package jcl.compiler.real.icg.generator.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.IfStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class IfCodeGenerator implements CodeGenerator<IfStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final IfStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final ClassWriter cw = currentClass.getClassWriter();
		MethodVisitor mv = currentClass.getMethodVisitor();

		mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "ifGen", "()V", null, null);
		mv.visitCode();
		// TODO: don't know if we need the above 2 lines...

		final Label loadTestForm = new Label();
		mv.visitLabel(loadTestForm);
//		mv.visitLineNumber(43, loadTestForm);
		final LispStruct testForm = input.getTestForm();
		formGenerator.generate(testForm, classBuilder);
		mv.visitVarInsn(Opcodes.ASTORE, 1);

		final Label resultForm = new Label();
		mv.visitLabel(resultForm);

		final Label elseLabel = new Label();

		final Label checkEqualsNullOrNIL = new Label();
		mv.visitLabel(checkEqualsNullOrNIL);
//		mv.visitLineNumber(46, checkEqualsNullOrNIL);

		mv.visitVarInsn(Opcodes.ALOAD, 1);
		mv.visitFieldInsn(Opcodes.GETSTATIC, "jcl/lists/NullStruct", "INSTANCE", "Ljcl/lists/NullStruct;");
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/lists/NullStruct", "equals", "(Ljava/lang/Object;)Z", false);
		mv.visitJumpInsn(Opcodes.IFNE, elseLabel);

		mv.visitVarInsn(Opcodes.ALOAD, 1);
		mv.visitFieldInsn(Opcodes.GETSTATIC, "jcl/symbols/NILStruct", "INSTANCE", "Ljcl/symbols/NILStruct;");
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/NILStruct", "equals", "(Ljava/lang/Object;)Z", false);
		mv.visitJumpInsn(Opcodes.IFNE, elseLabel);

		final Label loadThenForm = new Label();
		mv.visitLabel(loadThenForm);
//		mv.visitLineNumber(47, loadThenForm);
		final LispStruct thenForm = input.getThenForm();
		formGenerator.generate(thenForm, classBuilder);
		mv.visitVarInsn(Opcodes.ASTORE, 2);

		final Label ifEndLabel = new Label();
		mv.visitJumpInsn(Opcodes.GOTO, ifEndLabel);

		mv.visitLabel(elseLabel);
//		mv.visitLineNumber(49, elseLabel);
		mv.visitFrame(Opcodes.F_APPEND, 1, new Object[]{"jcl/LispStruct"}, 0, null);

		final LispStruct elseForm = input.getElseForm();
		formGenerator.generate(elseForm, classBuilder);
		mv.visitVarInsn(Opcodes.ASTORE, 2);

		mv.visitLabel(ifEndLabel);
//		mv.visitLineNumber(51, ifEndLabel);

		// TODO: don't know if the next lines are necessary. we might want to remain in the same method...
		mv.visitFrame(Opcodes.F_APPEND, 1, new Object[]{"jcl/LispStruct"}, 0, null);
		mv.visitVarInsn(Opcodes.ALOAD, 2);
		mv.visitInsn(Opcodes.ARETURN);

		final Label localVariables = new Label();
		mv.visitLabel(localVariables);
		mv.visitLocalVariable("testObj", "Ljcl/LispStruct;", null, checkEqualsNullOrNIL, localVariables, 1);
		mv.visitLocalVariable("result", "Ljcl/LispStruct;", null, resultForm, elseLabel, 2);
		mv.visitLocalVariable("result", "Ljcl/LispStruct;", null, ifEndLabel, localVariables, 2);

		// TODO: don't know if we need the next 2 lines
		mv.visitMaxs(3, 3);
		mv.visitEnd();
	}
}
