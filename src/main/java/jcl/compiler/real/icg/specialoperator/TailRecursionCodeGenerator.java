package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.FunctionCallCodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SymbolStruct;

public class TailRecursionCodeGenerator {

	// the list is of the form (%tail-recursion fn-symbol arg...)
	public static void genCodeTailRecursion(final IntermediateCodeGenerator icg, ListStruct list) {
		// drop the special operator
		list = list.getRest();
		// set up the proper function object (this)
		genCodeTailRecursionSetup(icg, (SymbolStruct) list.getFirst());
		// now set up the rest of the call just like any other fn call
		FunctionCallCodeGenerator.genCodeFunctionCall(icg, list, false);
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
