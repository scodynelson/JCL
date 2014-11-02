package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import org.objectweb.asm.Label;

public class UnwindProtectCodeGenerator {

	/**
	 * Transfer of Control Sequence for unwind-protect.
	 * http://clforjava.cs.cofc.edu/twiki/bin/view/CLJcompiler/TransferOfControl#Compilation_Time_TOC_Sequences_f
	 * ---------------------------
	 * 1. Start the try block
	 * 2. Setup for the evaluation of the protected form
	 * 3. Setup a goto for the finally block
	 * 4. Start the catch block
	 * 5. Using the exception, store the exception so that it can be rethrown after the cleanup form is executed
	 * 6. In the finally block, disable invalid TOC Exit Points
	 * 7. Setup a finally block
	 * 8. Setup the evalution of the cleanup form
	 * 9. Setup for a runtime call to process the return exception (if there was one, it will be rethrown automatically)
	 * 10. Register an exception handler for type Throwable
	 */
	public static void genCodeUnwindProtect(final IntermediateCodeGenerator icg, ListStruct list) {

		// Burn off the special symbol (UNWIND-PROTECT)
		list = list.getRest();

		//Get the protected form and the cleanup form(s) after the UNWIND-PROTECT symbol
		final ListStruct protectedForm = (ListStruct) list.getFirst();                //The protected form
		ListStruct cleanupForm = list.getRest();                  //The cleanup form

		//Create the exception table
		final Label startTryBlock = new Label();               //The start of the try block
		final Label catchBlock = new Label();                  //The start of the catch block
		final Label finallyBlock = new Label();                //The start of the finally block

		//1. Start the try block
		//Mark the start of the try block
		icg.emitter.visitMethodLabel(startTryBlock);

		//2. Setup for the evaluation of the protected form
		//Evalute the protected form
		icg.icgMainLoop(protectedForm);

		//3. Setup a goto for the finally block
		//If an exception wasn't thrown, go past the catch block to the finally block
		icg.emitter.emitGoto(finallyBlock);

		//4. Start the catch block
		//Start the catch block
		icg.emitter.visitMethodLabel(catchBlock);

		//5. Using the exception, store the exception so that it can be rethrown after the cleanup form is executed
		// ..., throw_excep
		icg.emitter.emitDup();
		// ..., throw_excep, throw_excep
		icg.emitter.emitInvokestatic("lisp/system/TransferOfControl", "setReturnException", "(Ljava/lang/Throwable;)", "V", false);
		// ..., throw_excep

		//6. In the finally block, disable invalid TOC Exit Points
		icg.emitter.emitInvokestatic("lisp/system/TransferOfControl", "disableExitPoints", "(Ljava/lang/Throwable;)", "V", false);
		// ...
		icg.emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");
		//7. Setup a finally block
		//Start the finally block
		icg.emitter.visitMethodLabel(finallyBlock);

		//8. Setup the evalution of the cleanup form
		//This is the finally code
		//Evalute the cleanup form
		while (!cleanupForm.equals(NullStruct.INSTANCE)) {
			icg.icgMainLoop(cleanupForm.getFirst());
			cleanupForm = cleanupForm.getRest();
			icg.emitter.emitPop();
		}

		//9. Setup for a runtime call to process the return exception (if there was one, it will be rethrown automatically)
		//Throw the stored exception if an exception was thrown in the protected form
		icg.emitter.emitInvokestatic("lisp/system/TransferOfControl", "processReturnException", "()", "V", false);

		//10. Register an exception handler for type Throwable
		icg.emitter.visitTryCatchBlock(
				startTryBlock,
				catchBlock,
				catchBlock,
				"java/lang/Throwable");
	}
}
