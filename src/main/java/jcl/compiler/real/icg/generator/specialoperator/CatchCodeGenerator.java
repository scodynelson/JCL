package jcl.compiler.real.icg.generator.specialoperator;

import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import org.objectweb.asm.Label;
import org.springframework.stereotype.Component;

@Component
public class CatchCodeGenerator implements CodeGenerator<ListStruct> {

	/**
	 * Implements the initial base code for a basis catch statement
	 */

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {

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

		//Create the exception table
		final Label startTryBlock = new Label();                //The start of the try block
		final Label catchBlock = new Label();                   //The start of the catch block
		final Label continueBlock = new Label();                //Subsequent code after the try/catch construct
		final Label ifBlock = new Label();                      //If block executed if this catches someone else's excepton

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
}
