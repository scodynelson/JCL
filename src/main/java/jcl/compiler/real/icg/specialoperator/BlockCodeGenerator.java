package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.symbols.SymbolStruct;
import org.objectweb.asm.Label;

public class BlockCodeGenerator {

	public static void genCodeBlock(final IntermediateCodeGenerator icg, ListStruct list) {

		final Label startTryBlock = new Label();                //The start of the try block
		final Label catchBlock = new Label();                   //The start of the catch block
		final Label continueBlock = new Label();                //Subsequent code after the try/catch construct
		final Label ifBlock = new Label();                      //If block executed if this catches someone else's excepton

		// Get rid of the BLOCK symbol
		list = list.getRest();
		final SymbolStruct<?> sym = (SymbolStruct) list.getFirst();
		list = list.getRest();

		// ... ,
		icg.genCodeSpecialVariable(sym);
		// ..., sym

		icg.emitter.emitGetstatic("lisp/system/TransferOfControl", "BLOCK", "Ljava/lang/String;");
		// ..., sym, BLOCK
		icg.emitter.emitSwap();
		// ... , BLOCK, sym
		icg.emitter.emitInvokestatic("lisp/system/TransferOfControl", "addTOCRecord", "(Ljava/lang/String;Ljava/lang/Object;)", "V", false);


        /* Call icgMainLoop() for each expression in the PROGN call,
		 * and remove all but the last expression's value from the stack  */
		icg.emitter.visitMethodLabel(startTryBlock);
		while (!list.equals(NullStruct.INSTANCE)) {
			icg.icgMainLoop(list.getFirst());
			list = list.getRest();
			if (!list.equals(NullStruct.INSTANCE)) {
				icg.emitter.emitNop();
				icg.emitter.emitPop();
				icg.emitter.emitNop();
			}
		}
		icg.emitter.emitGoto(continueBlock);

		//Start catch block
		icg.emitter.visitMethodLabel(catchBlock);
		// ..., throw_excep
		icg.emitter.emitDup();
		// ..., throw_excep, throw_excep
		icg.emitter.emitInvokestatic("lisp/system/TransferOfControl", "isMine", "(Ljava/lang/Throwable;)", "Ljava/lang/Object;", false);
		// ..., throw_excep, result
		icg.emitter.emitDup();
		// ..., throw_excep, result, result
		icg.emitter.emitIfnull(ifBlock);
		//Else block start
		// ..., throw_excep, result
		icg.emitter.emitSwap();
		// ..., result, throw_excep
		icg.emitter.emitPop();
		// ..., result
		icg.emitter.emitGoto(continueBlock);
		//Else block end

		//If block start
		icg.emitter.visitMethodLabel(ifBlock);
		// ..., throw_excep, result
		icg.emitter.emitSwap();
		// ..., result, throw_excep
		icg.emitter.emitInvokestatic("lisp/system/TransferOfControl", "setReturnException", "(Ljava/lang/Throwable;)", "V", false);
		// ..., result
		//If block end

		icg.emitter.visitMethodLabel(continueBlock);

		icg.emitter.visitTryCatchBlock(
				startTryBlock, //blockName + "_BlockA",
				catchBlock, //blockName + "_BlockB",
				catchBlock, //blockName + "_BlockB",
				"java/lang/Throwable");

		//Here is the finally code
		icg.emitter.emitInvokestatic("lisp/system/TransferOfControl", "popTOCRecord", "()", "V", false);

		icg.emitter.emitInvokestatic("lisp/system/TransferOfControl", "processReturnException", "()", "V", false);
	}
}
