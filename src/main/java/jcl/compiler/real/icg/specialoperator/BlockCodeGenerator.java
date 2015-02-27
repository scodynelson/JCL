package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.lists.NullStruct;
import jcl.system.EnhancedLinkedList;
import org.objectweb.asm.Label;

public class BlockCodeGenerator implements CodeGenerator<ConsElement> {

	public static final BlockCodeGenerator INSTANCE = new BlockCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {

		final Label startTryBlock = new Label();                //The start of the try block
		final Label catchBlock = new Label();                   //The start of the catch block
		final Label continueBlock = new Label();                //Subsequent code after the try/catch construct
		final Label ifBlock = new Label();                      //If block executed if this catches someone else's excepton

		// Get rid of the BLOCK symbol
		EnhancedLinkedList<SimpleElement> restOfList = input.getElements().getAllButFirst();
		final SymbolElement sym = (SymbolElement) restOfList.getFirst();
		restOfList = restOfList.getAllButFirst();

		// ... ,
		codeGenerator.genCodeSpecialVariable(sym);
		// ..., sym

		codeGenerator.emitter.emitGetstatic("lisp/system/TransferOfControl", "BLOCK", "Ljava/lang/String;");
		// ..., sym, BLOCK
		codeGenerator.emitter.emitSwap();
		// ... , BLOCK, sym
		codeGenerator.emitter.emitInvokestatic("lisp/system/TransferOfControl", "addTOCRecord", "(Ljava/lang/String;Ljava/lang/Object;)", "V", false);


        /* Call icgMainLoop() for each expression in the PROGN call,
		 * and remove all but the last expression's value from the stack  */
		codeGenerator.emitter.visitMethodLabel(startTryBlock);
		while (!restOfList.equals(NullStruct.INSTANCE)) {
			codeGenerator.icgMainLoop(restOfList.getFirst());
			restOfList = restOfList.getAllButFirst();
			if (!restOfList.equals(NullStruct.INSTANCE)) {
				codeGenerator.emitter.emitNop();
				codeGenerator.emitter.emitPop();
				codeGenerator.emitter.emitNop();
			}
		}
		codeGenerator.emitter.emitGoto(continueBlock);

		//Start catch block
		codeGenerator.emitter.visitMethodLabel(catchBlock);
		// ..., throw_excep
		codeGenerator.emitter.emitDup();
		// ..., throw_excep, throw_excep
		codeGenerator.emitter.emitInvokestatic("lisp/system/TransferOfControl", "isMine", "(Ljava/lang/Throwable;)", "Ljava/lang/Object;", false);
		// ..., throw_excep, result
		codeGenerator.emitter.emitDup();
		// ..., throw_excep, result, result
		codeGenerator.emitter.emitIfnull(ifBlock);
		//Else block start
		// ..., throw_excep, result
		codeGenerator.emitter.emitSwap();
		// ..., result, throw_excep
		codeGenerator.emitter.emitPop();
		// ..., result
		codeGenerator.emitter.emitGoto(continueBlock);
		//Else block end

		//If block start
		codeGenerator.emitter.visitMethodLabel(ifBlock);
		// ..., throw_excep, result
		codeGenerator.emitter.emitSwap();
		// ..., result, throw_excep
		codeGenerator.emitter.emitInvokestatic("lisp/system/TransferOfControl", "setReturnException", "(Ljava/lang/Throwable;)", "V", false);
		// ..., result
		//If block end

		codeGenerator.emitter.visitMethodLabel(continueBlock);

		codeGenerator.emitter.visitTryCatchBlock(
				startTryBlock, //blockName + "_BlockA",
				catchBlock, //blockName + "_BlockB",
				catchBlock, //blockName + "_BlockB",
				"java/lang/Throwable");

		//Here is the finally code
		codeGenerator.emitter.emitInvokestatic("lisp/system/TransferOfControl", "popTOCRecord", "()", "V", false);

		codeGenerator.emitter.emitInvokestatic("lisp/system/TransferOfControl", "processReturnException", "()", "V", false);
	}
}
