package jcl.compiler.real.icg.generator.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.struct.specialoperator.CatchStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.stereotype.Component;

@Component
public class CatchCodeGenerator implements CodeGenerator<ListStruct> {

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {

		//Create the exception table
		final Label startTryBlock = new Label();                //The start of the try block
		final Label catchBlock = new Label();                   //The start of the catch block
		final Label continueBlock = new Label();                //Subsequent code after the try/catch construct
		final Label ifBlock = new Label();                      //If block executed if this catches someone else's excepton

		// Burn off the special symbol (CATCH)
		ListStruct restOfList = input.getRest();
		//Get the catchTag and set up for runtime eval of the catchTag
		final Object catchTag = restOfList.getFirst();                    //The first parameter to CATCH that must first be evaluated
		restOfList = restOfList.getRest();

		// ... ,
		codeGenerator.icgMainLoop(catchTag, classBuilder);
		// ..., catchTag

		classBuilder.getEmitter().emitGetstatic("lisp/system/TransferOfControl", "CATCH", "Ljava/lang/String;");
		// ..., catchTag, CATCH
		classBuilder.getEmitter().emitSwap();
		// ... , CATCH, catchTag
		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "addTOCRecord", "(Ljava/lang/String;Ljava/lang/Object;)", "V", false);

		//Mark the start of the try block
		classBuilder.getEmitter().visitMethodLabel(startTryBlock);

		//Evalute the rest of the list
		while (!restOfList.equals(NullStruct.INSTANCE)) {
			codeGenerator.icgMainLoop(restOfList.getFirst(), classBuilder);
			restOfList = restOfList.getRest();
			if (!restOfList.equals(NullStruct.INSTANCE)) {
				classBuilder.getEmitter().emitPop();
			}
		}

		//If an exception wasn't thrown, go past the catch block to the finally block
		classBuilder.getEmitter().emitGoto(continueBlock);

		//Start the catch block
		classBuilder.getEmitter().visitMethodLabel(catchBlock);
		// ..., throw_excep
		classBuilder.getEmitter().emitDup();
		// ..., throw_excep, throw_excep
		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "isMine", "(Ljava/lang/Throwable;)", "Ljava/lang/Object;", false);
		// ..., throw_excep, result
		classBuilder.getEmitter().emitDup();
		// ..., throw_excep, result, result
		classBuilder.getEmitter().emitIfnull(ifBlock);
		//Else block start
		// ..., throw_excep, result
		classBuilder.getEmitter().emitSwap();
		// ..., result, throw_excep
		classBuilder.getEmitter().emitPop();
		// ..., result
		classBuilder.getEmitter().emitGoto(continueBlock);
		//Else block end

		//If block start
		classBuilder.getEmitter().visitMethodLabel(ifBlock);
		// ..., throw_excep, result
		classBuilder.getEmitter().emitSwap();
		// ..., result, throw_excep
		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "setReturnException", "(Ljava/lang/Throwable;)", "V", false);
		// ..., result
		//If block end

		//Signify the remainder of the code block
		classBuilder.getEmitter().visitMethodLabel(continueBlock);

		classBuilder.getEmitter().visitTryCatchBlock(
				startTryBlock,
				catchBlock,
				catchBlock,
				"java/lang/Throwable");
		//"lisp/system/compiler/exceptions/ThrowException");

		//Here is the finally code
		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "popTOCRecord", "()", "V", false);

		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "processReturnException", "()", "V", false);
	}

	public void dump(final CatchStruct catchStruct, final ClassWriter cw, MethodVisitor mv) {

		mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "catchGen", "()Ljava/lang/Object;", null, null);
		mv.visitCode();
		// TODO: don't know if we need the above 2 lines...

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlock = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlock, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException");

		final LispStruct catchTag = catchStruct.getCatchTag();

		final Label getCatchTagValue = new Label();
		mv.visitLabel(getCatchTagValue);
//		mv.visitLineNumber(61, getCatchTagValue);
		//**** TODO: START IGC LOOP CALL ON CATCH-TAG ****//
		mv.visitTypeInsn(Opcodes.NEW, "jcl/characters/CharacterStruct");
		mv.visitInsn(Opcodes.DUP);
		mv.visitIntInsn(Opcodes.BIPUSH, 97);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/characters/CharacterStruct", "<init>", "(I)V", false);
		//**** TODO: END IGC LOOP CALL ON CATCH-TAG ****//

		mv.visitVarInsn(Opcodes.ASTORE, 1);

		final List<LispStruct> forms = catchStruct.getForms();

		mv.visitLabel(tryBlockStart);
//		mv.visitLineNumber(65, tryBlockStart);
		//**** TODO: START IGC LOOP CALL ON FORMS ****//
		mv.visitTypeInsn(Opcodes.NEW, "jcl/characters/CharacterStruct");
		mv.visitInsn(Opcodes.DUP);
		mv.visitIntInsn(Opcodes.SIPUSH, 197);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/characters/CharacterStruct", "<init>", "(I)V", false);
		//**** TODO: END IGC LOOP CALL ON FORMS ****//

		mv.visitVarInsn(Opcodes.ASTORE, 2);

		mv.visitLabel(tryBlockEnd);
//		mv.visitLineNumber(72, tryBlockEnd);
		final Label catchBlockEnd = new Label();
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlock);
//		mv.visitLineNumber(66, catchBlock);
		mv.visitFrame(Opcodes.F_FULL, 1, new Object[]{"jcl/LispStruct"}, 1, new Object[]{"jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException"});
		mv.visitVarInsn(Opcodes.ASTORE, 3);

		final Label getThrowCatchTag = new Label();
		mv.visitLabel(getThrowCatchTag);
//		mv.visitLineNumber(67, getThrowCatchTag);
		mv.visitVarInsn(Opcodes.ALOAD, 3);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException", "getCatchTag", "()Ljcl/LispStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, 4);

		final Label checkEqualsThrowCatchTagAndCatchTag = new Label();
		mv.visitLabel(checkEqualsThrowCatchTagAndCatchTag);
//		mv.visitLineNumber(68, checkEqualsThrowCatchTagAndCatchTag);
		mv.visitVarInsn(Opcodes.ALOAD, 4);
		mv.visitVarInsn(Opcodes.ALOAD, 1);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false);

		final Label setResultValue = new Label();
		mv.visitJumpInsn(Opcodes.IFNE, setResultValue);

		final Label rethrowThrowException = new Label();
		mv.visitLabel(rethrowThrowException);
//		mv.visitLineNumber(69, rethrowThrowException);
		mv.visitVarInsn(Opcodes.ALOAD, 3);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(setResultValue);
//		mv.visitLineNumber(71, setResultValue);
		mv.visitFrame(Opcodes.F_APPEND, 3, new Object[]{Opcodes.TOP, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException", "jcl/LispStruct"}, 0, null);
		mv.visitVarInsn(Opcodes.ALOAD, 3);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/icg/generator/specialoperator/exception/ThrowException", "getResultForm", "()Ljcl/LispStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, 2);

		mv.visitLabel(catchBlockEnd);
//		mv.visitLineNumber(73, catchBlockEnd);
		mv.visitFrame(Opcodes.F_FULL, 2, new Object[]{"jcl/LispStruct", "jcl/LispStruct"}, 0, new Object[]{});
		mv.visitVarInsn(Opcodes.ALOAD, 2);

		// TODO: don't know if the next line is necessary. we might want to remain in the same method...
		mv.visitInsn(Opcodes.ARETURN);

		final Label localVariables = new Label();
		mv.visitLabel(localVariables);
		mv.visitLocalVariable("catchTag", "Ljcl/LispStruct;", null, tryBlockStart, localVariables, 1);
		mv.visitLocalVariable("resultForm", "Ljcl/LispStruct;", null, tryBlockEnd, catchBlock, 2);
		mv.visitLocalVariable("resultForm", "Ljcl/LispStruct;", null, catchBlockEnd, localVariables, 2);
		mv.visitLocalVariable("te", "Ljcl/compiler/real/icg/generator/specialoperator/exception/ThrowException;", null, getThrowCatchTag, catchBlockEnd, 3);
		mv.visitLocalVariable("teCatchTag", "Ljcl/LispStruct;", null, checkEqualsThrowCatchTagAndCatchTag, catchBlockEnd, 4);

		// TODO: don't know if we need the next 2 lines
		mv.visitMaxs(3, 4);
		mv.visitEnd();
	}
}
