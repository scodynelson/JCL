package jcl.compiler.real.icg.specialoperator.compiler;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.FunctionCallCodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.symbols.SymbolStruct;
import jcl.system.EnhancedLinkedList;

public class TailRecursionCodeGenerator implements CodeGenerator<ConsElement> {

	// the list is of the form (%tail-recursion fn-symbol arg...)

	public static final TailRecursionCodeGenerator INSTANCE = new TailRecursionCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {
		// drop the special operator
		final EnhancedLinkedList<SimpleElement> restOfList = input.getElements().getAllButFirst();
		// set up the proper function object (this)
		genCodeTailRecursionSetup(codeGenerator, (SymbolStruct) restOfList.getFirst());
		// now set up the rest of the call just like any other fn call
		final boolean acceptsMultipleValues = FunctionCallCodeGenerator.INSTANCE.isAcceptsMultipleValues();
		try {
			FunctionCallCodeGenerator.INSTANCE.setAcceptsMultipleValues(false);
			FunctionCallCodeGenerator.INSTANCE.generate(new ConsElement(restOfList), codeGenerator);
		} finally {
			FunctionCallCodeGenerator.INSTANCE.setAcceptsMultipleValues(acceptsMultipleValues);
		}
	}

	/**
	 * This method handles a simple tail recursion. Instead of looking up a function,
	 * either by symbol name or from the list of std CL functions, the method
	 * just sets up to call the enclosing function's funcall or apply method. Since the
	 * enclosing function is the current object, the method only generates an ALOAD 0 -
	 * the reference to 'this'.
	 */
	private static void genCodeTailRecursionSetup(final IntermediateCodeGenerator icg, final SymbolStruct<?> sym) {
		icg.emitter.emitAload(0);
	}
}
