package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import org.objectweb.asm.Label;

public class CatchCodeGenerator {

	/**
	 * Implements the initial base code for a basis catch statement
	 */
	public static void genCodeCatch(final IntermediateCodeGenerator icg, ListStruct list) {

		// Burn off the special symbol (CATCH)
		list = list.getRest();

		//Get the catchTag and set up for runtime eval of the catchTag
		final Object catchTag = list.getFirst();                    //The first parameter to CATCH that must first be evaluated
		list = list.getRest();

		// ... ,
		icg.icgMainLoop(catchTag);
		// ..., catchTag

		icg.emitter.emitGetstatic("lisp/system/TransferOfControl", "CATCH", "Ljava/lang/String;");
		// ..., catchTag, CATCH
		icg.emitter.emitSwap();
		// ... , CATCH, catchTag
		icg.emitter.emitInvokestatic("lisp/system/TransferOfControl", "addTOCRecord", "(Ljava/lang/String;Ljava/lang/Object;)", "V", false);

		//Create the exception table
		final Label startTryBlock = new Label();                //The start of the try block
		final Label catchBlock = new Label();                   //The start of the catch block
		final Label continueBlock = new Label();                //Subsequent code after the try/catch construct
		final Label ifBlock = new Label();                      //If block executed if this catches someone else's excepton

		//Mark the start of the try block
		icg.emitter.visitMethodLabel(startTryBlock);

		//Evalute the rest of the list
		while (!list.equals(NullStruct.INSTANCE)) {
			icg.icgMainLoop(list.getFirst());
			list = list.getRest();
			if (!list.equals(NullStruct.INSTANCE)) {
				icg.emitter.emitPop();
			}
		}

		//If an exception wasn't thrown, go past the catch block to the finally block
		icg.emitter.emitGoto(continueBlock);

		//Start the catch block
		icg.emitter.visitMethodLabel(catchBlock);
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

		//Signify the remainder of the code block
		icg.emitter.visitMethodLabel(continueBlock);

		icg.emitter.visitTryCatchBlock(
				startTryBlock,
				catchBlock,
				catchBlock,
				"java/lang/Throwable");
		//"lisp/system/compiler/exceptions/ThrowException");

		//Here is the finally code
		icg.emitter.emitInvokestatic("lisp/system/TransferOfControl", "popTOCRecord", "()", "V", false);

		icg.emitter.emitInvokestatic("lisp/system/TransferOfControl", "processReturnException", "()", "V", false);
	}
}
