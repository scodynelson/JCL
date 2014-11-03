package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import org.objectweb.asm.Label;

public class CatchCodeGenerator implements CodeGenerator<ListStruct> {

	/**
	 * Implements the initial base code for a basis catch statement
	 */

	public static final CatchCodeGenerator INSTANCE = new CatchCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator) {

		// Burn off the special symbol (CATCH)
		ListStruct restOfList = input.getRest();

		//Get the catchTag and set up for runtime eval of the catchTag
		final Object catchTag = restOfList.getFirst();                    //The first parameter to CATCH that must first be evaluated
		restOfList = restOfList.getRest();

		// ... ,
		codeGenerator.icgMainLoop(catchTag);
		// ..., catchTag

		codeGenerator.emitter.emitGetstatic("lisp/system/TransferOfControl", "CATCH", "Ljava/lang/String;");
		// ..., catchTag, CATCH
		codeGenerator.emitter.emitSwap();
		// ... , CATCH, catchTag
		codeGenerator.emitter.emitInvokestatic("lisp/system/TransferOfControl", "addTOCRecord", "(Ljava/lang/String;Ljava/lang/Object;)", "V", false);

		//Create the exception table
		final Label startTryBlock = new Label();                //The start of the try block
		final Label catchBlock = new Label();                   //The start of the catch block
		final Label continueBlock = new Label();                //Subsequent code after the try/catch construct
		final Label ifBlock = new Label();                      //If block executed if this catches someone else's excepton

		//Mark the start of the try block
		codeGenerator.emitter.visitMethodLabel(startTryBlock);

		//Evalute the rest of the list
		while (!restOfList.equals(NullStruct.INSTANCE)) {
			codeGenerator.icgMainLoop(restOfList.getFirst());
			restOfList = restOfList.getRest();
			if (!restOfList.equals(NullStruct.INSTANCE)) {
				codeGenerator.emitter.emitPop();
			}
		}

		//If an exception wasn't thrown, go past the catch block to the finally block
		codeGenerator.emitter.emitGoto(continueBlock);

		//Start the catch block
		codeGenerator.emitter.visitMethodLabel(catchBlock);
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

		//Signify the remainder of the code block
		codeGenerator.emitter.visitMethodLabel(continueBlock);

		codeGenerator.emitter.visitTryCatchBlock(
				startTryBlock,
				catchBlock,
				catchBlock,
				"java/lang/Throwable");
		//"lisp/system/compiler/exceptions/ThrowException");

		//Here is the finally code
		codeGenerator.emitter.emitInvokestatic("lisp/system/TransferOfControl", "popTOCRecord", "()", "V", false);

		codeGenerator.emitter.emitInvokestatic("lisp/system/TransferOfControl", "processReturnException", "()", "V", false);
	}
}
