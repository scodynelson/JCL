package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;

public class ThrowCodeGenerator implements CodeGenerator<ListStruct> {


	public static final ThrowCodeGenerator INSTANCE = new ThrowCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator) {

		// Remove the special symbol (THROW) from the list
		ListStruct restOfList = input.getRest();

		//Get the catch tag and store for later evaluation
		final Object catchTag = restOfList.getFirst();         //The catch tag value that must be evaluated
		restOfList = restOfList.getRest();


		codeGenerator.emitter.emitNew("lisp/system/compiler/exceptions/ThrowException");
		// +1 -> exception
		codeGenerator.emitter.emitDup();
		// +2 -> exception, exception

		//Run the catch tag through the compiler for eval with the intent that the result
		//will be on the stack ready to be a parameter to the following method invokation
		codeGenerator.icgMainLoop(catchTag);
		// +3 -> exception, exception, name
		codeGenerator.icgMainLoop(restOfList.getFirst());
		codeGenerator.emitter.emitInvokespecial("lisp/system/compiler/exceptions/ThrowException", "<init>", "(Ljava/lang/Object;Ljava/lang/Object;)", "V", false);
		codeGenerator.emitter.emitAthrow();
	}
}
