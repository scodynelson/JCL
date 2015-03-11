package jcl.compiler.real.icg.specialoperator.compiler;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.FunctionCallCodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TailRecursionCodeGenerator implements CodeGenerator<ListStruct> {

	// the list is of the form (%tail-recursion fn-symbol arg...)

	@Autowired
	private FunctionCallCodeGenerator functionCallCodeGenerator;

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		// drop the special operator
		final ListStruct restOfList = input.getRest();
		// set up the proper function object (this)
		genCodeTailRecursionSetup(codeGenerator, (SymbolStruct) restOfList.getFirst(), classBuilder);
		// now set up the rest of the call just like any other fn call
		final boolean acceptsMultipleValues = functionCallCodeGenerator.isAcceptsMultipleValues();
		try {
			functionCallCodeGenerator.setAcceptsMultipleValues(false);
			functionCallCodeGenerator.generate(restOfList, codeGenerator, classBuilder);
		} finally {
			functionCallCodeGenerator.setAcceptsMultipleValues(acceptsMultipleValues);
		}
	}

	/**
	 * This method handles a simple tail recursion. Instead of looking up a function,
	 * either by symbol name or from the list of std CL functions, the method
	 * just sets up to call the enclosing function's funcall or apply method. Since the
	 * enclosing function is the current object, the method only generates an ALOAD 0 -
	 * the reference to 'this'.
	 */
	private static void genCodeTailRecursionSetup(final IntermediateCodeGenerator icg, final SymbolStruct<?> sym, final JavaClassBuilder classBuilder) {
		classBuilder.getEmitter().emitAload(0);
	}
}
