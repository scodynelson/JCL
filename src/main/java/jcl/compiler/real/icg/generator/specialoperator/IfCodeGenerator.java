package jcl.compiler.real.icg.generator.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.IfStruct;
import jcl.lists.ListStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class IfCodeGenerator implements CodeGenerator<ListStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final ListStruct input, final JavaClassBuilder classBuilder) {

		ListStruct restOfList = input.getRest();
		final LispStruct testObj = restOfList.getFirst();
		restOfList = restOfList.getRest();
		final LispStruct thenObj = restOfList.getFirst();
		restOfList = restOfList.getRest();
		final LispStruct elseObj = restOfList.getFirst();

		formGenerator.generate(testObj, classBuilder);
		final Label outLabel = new Label();
		classBuilder.getEmitter().emitDup();
		classBuilder.getEmitter().emitInstanceof("[Ljava/lang/Object;");
		classBuilder.getEmitter().emitIfeq(outLabel);
		classBuilder.getEmitter().emitCheckcast("[Ljava/lang/Object;");
		classBuilder.getEmitter().emitLdc(0);
		classBuilder.getEmitter().emitAaload();
		classBuilder.getEmitter().visitMethodLabel(outLabel);
		classBuilder.getEmitter().emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");

		final Label thenLabel = new Label();
		final Label elseLabel = new Label();
		final Label endLabel = new Label();

		classBuilder.getEmitter().emitIf_acmpeq(elseLabel);
		classBuilder.getEmitter().visitMethodLabel(thenLabel);
		formGenerator.generate(thenObj, classBuilder);
		classBuilder.getEmitter().emitGoto(endLabel);

		classBuilder.getEmitter().visitMethodLabel(elseLabel);
		formGenerator.generate(elseObj, classBuilder);

		classBuilder.getEmitter().visitMethodLabel(endLabel);
	}

	public void dump(final IfStruct ifStruct, final ClassWriter cw, MethodVisitor mv) {

		mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "ifGen", "()Ljava/lang/Object;", null, null);
		mv.visitCode();
		// TODO: don't know if we need the above 2 lines...

		final LispStruct testForm = ifStruct.getTestForm();

		final Label loadTestForm = new Label();
		mv.visitLabel(loadTestForm);
//		mv.visitLineNumber(43, loadTestForm);
		//**** TODO: START IGC LOOP CALL ON TEST-FORM ****//
		mv.visitInsn(Opcodes.ACONST_NULL);
		//**** TODO: END IGC LOOP CALL ON TEST-FORM ****//
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

		final LispStruct thenForm = ifStruct.getThenForm();

		final Label loadThenForm = new Label();
		mv.visitLabel(loadThenForm);
//		mv.visitLineNumber(47, loadThenForm);

		//**** TODO: START IGC LOOP CALL ON THEN-FORM ****//
		mv.visitTypeInsn(Opcodes.NEW, "jcl/characters/CharacterStruct");
		mv.visitInsn(Opcodes.DUP);
		mv.visitIntInsn(Opcodes.BIPUSH, 97);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/characters/CharacterStruct", "<init>", "(I)V", false);
		//**** TODO: END IGC LOOP CALL ON THEN-FORM ****//

		mv.visitVarInsn(Opcodes.ASTORE, 2);

		final Label ifEndLabel = new Label();
		mv.visitJumpInsn(Opcodes.GOTO, ifEndLabel);

		mv.visitLabel(elseLabel);
//		mv.visitLineNumber(49, elseLabel);
		mv.visitFrame(Opcodes.F_APPEND, 1, new Object[]{"jcl/LispStruct"}, 0, null);

		final LispStruct elseForm = ifStruct.getElseForm();

		//**** TODO: START IGC LOOP CALL ON ELSE-FORM ****//
		mv.visitTypeInsn(Opcodes.NEW, "jcl/characters/CharacterStruct");
		mv.visitInsn(Opcodes.DUP);
		mv.visitIntInsn(Opcodes.SIPUSH, 197);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/characters/CharacterStruct", "<init>", "(I)V", false);
		//**** TODO: END IGC LOOP CALL ON ELSE-FORM ****//

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
