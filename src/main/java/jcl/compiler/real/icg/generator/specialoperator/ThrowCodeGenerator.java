package jcl.compiler.real.icg.generator.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.struct.specialoperator.ThrowStruct;
import jcl.lists.ListStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class ThrowCodeGenerator implements CodeGenerator<ListStruct> {

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {

		// Remove the special symbol (THROW) from the list
		ListStruct restOfList = input.getRest();

		//Get the catch tag and store for later evaluation
		final Object catchTag = restOfList.getFirst();         //The catch tag value that must be evaluated
		restOfList = restOfList.getRest();

		classBuilder.getEmitter().emitNew("lisp/system/compiler/exceptions/ThrowException");
		// +1 -> exception
		classBuilder.getEmitter().emitDup();
		// +2 -> exception, exception

		//Run the catch tag through the compiler for eval with the intent that the result
		//will be on the stack ready to be a parameter to the following method invokation
		codeGenerator.icgMainLoop(catchTag, classBuilder);
		// +3 -> exception, exception, name
		codeGenerator.icgMainLoop(restOfList.getFirst(), classBuilder);
		classBuilder.getEmitter().emitInvokespecial("lisp/system/compiler/exceptions/ThrowException", "<init>", "(Ljava/lang/Object;Ljava/lang/Object;)", "V", false);
		classBuilder.getEmitter().emitAthrow();
	}

	public void dump(final ThrowStruct throwStruct, final ClassWriter cw, MethodVisitor mv) {

		mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "throwGen", "()V", null, null);
		mv.visitCode();
		// TODO: don't know if we need the above 2 lines...

		final LispStruct catchTag = throwStruct.getCatchTag();

		final Label getCatchTagValue = new Label();
		mv.visitLabel(getCatchTagValue);
//		mv.visitLineNumber(78, getCatchTagValue);
		//**** TODO: START IGC LOOP CALL ON FORMS ****//
		mv.visitTypeInsn(Opcodes.NEW, "jcl/characters/CharacterStruct");
		mv.visitInsn(Opcodes.DUP);
		mv.visitIntInsn(Opcodes.BIPUSH, 97);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/characters/CharacterStruct", "<init>", "(I)V", false);
		//**** TODO: END IGC LOOP CALL ON FORMS ****//

		mv.visitVarInsn(Opcodes.ASTORE, 1);

		final LispStruct resultForm = throwStruct.getResultForm();

		final Label getResultFormValue = new Label();
		mv.visitLabel(getResultFormValue);
//		mv.visitLineNumber(79, getResultFormValue);
		//**** TODO: START IGC LOOP CALL ON RESULT ****//
		mv.visitTypeInsn(Opcodes.NEW, "jcl/characters/CharacterStruct");
		mv.visitInsn(Opcodes.DUP);
		mv.visitIntInsn(Opcodes.BIPUSH, 97);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/characters/CharacterStruct", "<init>", "(I)V", false);
		//**** TODO: END IGC LOOP CALL ON RESULT ****//

		mv.visitVarInsn(Opcodes.ASTORE, 2);

		final Label throwThrowException = new Label();
		mv.visitLabel(throwThrowException);
//		mv.visitLineNumber(81, throwThrowException);
		mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException");
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, 1);
		mv.visitVarInsn(Opcodes.ALOAD, 2);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException", "<init>", "(Ljcl/LispStruct;Ljcl/LispStruct;)V", false);
		mv.visitInsn(Opcodes.ATHROW);

		final Label localVariables = new Label();
		mv.visitLabel(localVariables);
		mv.visitLocalVariable("getCatchTagValue", "Ljcl/LispStruct;", null, getResultFormValue, localVariables, 1);
		mv.visitLocalVariable("resultForm", "Ljcl/LispStruct;", null, throwThrowException, localVariables, 2);

		// TODO: don't know if we need the next 2 lines
		mv.visitMaxs(4, 2);
		mv.visitEnd();
	}
}
