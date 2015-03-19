package jcl.compiler.real.icg.generator.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.SpecialVariableCodeGenerator;
import jcl.compiler.real.struct.specialoperator.BlockStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class BlockCodeGenerator implements CodeGenerator<ListStruct> {

	@Autowired
	private SpecialVariableCodeGenerator specialVariableCodeGenerator;

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {

		final Label startTryBlock = new Label();                //The start of the try block
		final Label catchBlock = new Label();                   //The start of the catch block
		final Label continueBlock = new Label();                //Subsequent code after the try/catch construct
		final Label ifBlock = new Label();                      //If block executed if this catches someone else's excepton

		// Get rid of the BLOCK symbol
		ListStruct restOfList = input.getRest();
		final SymbolStruct<?> sym = (SymbolStruct<?>) restOfList.getFirst();
		restOfList = restOfList.getRest();

		// ... ,
		specialVariableCodeGenerator.generate(sym, codeGenerator, classBuilder);
		// ..., sym

		classBuilder.getEmitter().emitGetstatic("lisp/system/TransferOfControl", "BLOCK", "Ljava/lang/String;");
		// ..., sym, BLOCK
		classBuilder.getEmitter().emitSwap();
		// ... , BLOCK, sym
		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "addTOCRecord", "(Ljava/lang/String;Ljava/lang/Object;)", "V", false);

		classBuilder.getEmitter().visitMethodLabel(startTryBlock);

        /* Call icgMainLoop() for each expression in the PROGN call,
		 * and remove all but the last expression's value from the stack  */
		while (!restOfList.equals(NullStruct.INSTANCE)) {
			codeGenerator.icgMainLoop(restOfList.getFirst(), classBuilder);
			restOfList = restOfList.getRest();
			if (!restOfList.equals(NullStruct.INSTANCE)) {
				classBuilder.getEmitter().emitNop();
				classBuilder.getEmitter().emitPop();
				classBuilder.getEmitter().emitNop();
			}
		}
		classBuilder.getEmitter().emitGoto(continueBlock);

		//Start catch block
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

		classBuilder.getEmitter().visitMethodLabel(continueBlock);

		classBuilder.getEmitter().visitTryCatchBlock(
				startTryBlock, //blockName + "_BlockA",
				catchBlock, //blockName + "_BlockB",
				catchBlock, //blockName + "_BlockB",
				"java/lang/Throwable");

		//Here is the finally code
		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "popTOCRecord", "()", "V", false);

		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "processReturnException", "()", "V", false);
	}

	public void dump(final BlockStruct blockStruct, final ClassWriter cw, MethodVisitor mv) {

		mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "block", "()Ljava/lang/Object;", null, null);
		mv.visitCode();
		// TODO: don't know if we need the above 2 lines...

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlock = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlock, "jcl/compiler/real/icg/generator/specialoperator/exception/ReturnFromException");

		final SymbolStruct<?> name = blockStruct.getName();

		final Label namePackage = new Label();
		mv.visitLabel(namePackage);
//		mv.visitLineNumber(19, namePackage);
		final String packageName = name.getSymbolPackage().getName();
		mv.visitLdcInsn(packageName);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/packages/PackageStruct", "findPackage", "(Ljava/lang/String;)Ljcl/packages/PackageStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, 1);

		final Label nameSymbol = new Label();
		mv.visitLabel(nameSymbol);
//		mv.visitLineNumber(20, nameSymbol);
		mv.visitVarInsn(Opcodes.ALOAD, 1);
		final String symbolName = name.getName();
		mv.visitLdcInsn(symbolName);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageStruct", "findSymbol", "(Ljava/lang/String;)Ljcl/packages/PackageSymbolStruct;", false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/packages/PackageSymbolStruct", "getSymbol", "()Ljcl/symbols/SymbolStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, 2);

		final List<LispStruct> forms = blockStruct.getForms();

		mv.visitLabel(tryBlockStart);
//		mv.visitLineNumber(24, tryBlockStart);
		//**** TODO: START IGC LOOP CALL ON FORMS ****//
		mv.visitTypeInsn(Opcodes.NEW, "jcl/characters/CharacterStruct");
		mv.visitInsn(Opcodes.DUP);
		mv.visitIntInsn(Opcodes.BIPUSH, 97);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/characters/CharacterStruct", "<init>", "(I)V", false);
		//**** TODO: END IGC LOOP CALL ON FORMS ****//

		mv.visitVarInsn(Opcodes.ASTORE, 3);

		mv.visitLabel(tryBlockEnd);
//		mv.visitLineNumber(31, tryBlockEnd);
		final Label catchBlockEnd = new Label();
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlock);
//		mv.visitLineNumber(25, catchBlock);
		mv.visitFrame(Opcodes.F_FULL, 2, new Object[]{"jcl/packages/PackageStruct", "jcl/symbols/SymbolStruct"}, 1, new Object[]{"jcl/compiler/real/icg/generator/specialoperator/exception/ReturnFromException"});
		mv.visitVarInsn(Opcodes.ASTORE, 4);

		final Label getReturnFromName = new Label();
		mv.visitLabel(getReturnFromName);
//		mv.visitLineNumber(26, getReturnFromName);
		mv.visitVarInsn(Opcodes.ALOAD, 4);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/icg/generator/specialoperator/exception/ReturnFromException", "getName", "()Ljcl/symbols/SymbolStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, 5);

		final Label checkEqualsReturnFromNameAndBlockName = new Label();
		mv.visitLabel(checkEqualsReturnFromNameAndBlockName);
//		mv.visitLineNumber(27, checkEqualsReturnFromNameAndBlockName);
		mv.visitVarInsn(Opcodes.ALOAD, 5);
		mv.visitVarInsn(Opcodes.ALOAD, 2);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/symbols/SymbolStruct", "equals", "(Ljava/lang/Object;)Z", false);

		final Label setResultValue = new Label();
		mv.visitJumpInsn(Opcodes.IFNE, setResultValue);

		final Label rethrowReturnFromException = new Label();
		mv.visitLabel(rethrowReturnFromException);
//		mv.visitLineNumber(28, rethrowReturnFromException);
		mv.visitVarInsn(Opcodes.ALOAD, 4);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(setResultValue);
//		mv.visitLineNumber(30, setResultValue);
		mv.visitFrame(Opcodes.F_APPEND, 3, new Object[]{Opcodes.TOP, "jcl/compiler/real/icg/generator/specialoperator/exception/ReturnFromException", "jcl/symbols/SymbolStruct"}, 0, null);
		mv.visitVarInsn(Opcodes.ALOAD, 4);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/icg/generator/specialoperator/exception/ReturnFromException", "getResult", "()Ljcl/LispStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, 3);

		mv.visitLabel(catchBlockEnd);
//		mv.visitLineNumber(32, catchBlockEnd);
		mv.visitFrame(Opcodes.F_FULL, 3, new Object[]{"jcl/packages/PackageStruct", "jcl/symbols/SymbolStruct", "jcl/LispStruct"}, 0, new Object[]{});
		mv.visitVarInsn(Opcodes.ALOAD, 3);

		// TODO: don't know if the next line is necessary. we might want to remain in the same method...
		mv.visitInsn(Opcodes.ARETURN);

		final Label localVariables = new Label();
		mv.visitLabel(localVariables);
		mv.visitLocalVariable("pkg", "Ljcl/packages/PackageStruct;", null, nameSymbol, localVariables, 1);
		mv.visitLocalVariable("name", "Ljcl/symbols/SymbolStruct;", "Ljcl/symbols/SymbolStruct<*>;", tryBlockStart, localVariables, 2);
		mv.visitLocalVariable("result", "Ljcl/LispStruct;", null, tryBlockEnd, catchBlock, 3);
		mv.visitLocalVariable("result", "Ljcl/LispStruct;", null, catchBlockEnd, localVariables, 3);
		mv.visitLocalVariable("rte", "Ljcl/compiler/real/icg/generator/specialoperator/exception/ReturnFromException;", null, getReturnFromName, catchBlockEnd, 4);
		mv.visitLocalVariable("rteName", "Ljcl/symbols/SymbolStruct;", "Ljcl/symbols/SymbolStruct<*>;", checkEqualsReturnFromNameAndBlockName, catchBlockEnd, 5);

		// TODO: don't know if we need the next 2 lines
		mv.visitMaxs(3, 5);
		mv.visitEnd();
	}
}
