package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;

public class ThrowCodeGenerator implements CodeGenerator<ListStruct> {

	public static final ThrowCodeGenerator INSTANCE = new ThrowCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {

		// Remove the special symbol (THROW) from the list
		ListStruct restOfList = input.getRest();

		//Get the catch tag and store for later evaluation
		final Object catchTag = restOfList.getFirst();         //The catch tag value that must be evaluated
		restOfList = restOfList.getRest();


		classBuilder.getEmitter().emitNew("lisp/system/compiler/exceptions/ThrowException");
		// +1 -> exception
		classBuilder.getEmitter().emitDup();
		// +2 -> exception, exception

		//Run the catch tag through the compiler for eval with the intent that the result
		//will be on the stack ready to be a parameter to the following method invokation
		codeGenerator.icgMainLoop(catchTag, classBuilder);
		// +3 -> exception, exception, name
		codeGenerator.icgMainLoop(restOfList.getFirst(), classBuilder);
		classBuilder.getEmitter().emitInvokespecial("lisp/system/compiler/exceptions/ThrowException", "<init>", "(Ljava/lang/Object;Ljava/lang/Object;)", "V", false);
		classBuilder.getEmitter().emitAthrow();
	}
}
