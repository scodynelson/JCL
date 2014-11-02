package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;

public class ThrowCodeGenerator {

	public static void genCodeThrow(final IntermediateCodeGenerator icg, ListStruct list) {

		// Remove the special symbol (THROW) from the list
		list = list.getRest();

		//Get the catch tag and store for later evaluation
		final Object catchTag = list.getFirst();         //The catch tag value that must be evaluated
		list = list.getRest();


		icg.emitter.emitNew("lisp/system/compiler/exceptions/ThrowException");
		// +1 -> exception
		icg.emitter.emitDup();
		// +2 -> exception, exception

		//Run the catch tag through the compiler for eval with the intent that the result
		//will be on the stack ready to be a parameter to the following method invokation
		icg.icgMainLoop(catchTag);
		// +3 -> exception, exception, name
		icg.icgMainLoop(list.getFirst());
		icg.emitter.emitInvokespecial("lisp/system/compiler/exceptions/ThrowException", "<init>", "(Ljava/lang/Object;Ljava/lang/Object;)", "V", false);
		icg.emitter.emitAthrow();
	}
}
