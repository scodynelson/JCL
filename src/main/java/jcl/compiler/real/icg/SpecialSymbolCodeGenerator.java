package jcl.compiler.real.icg;

import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.symbols.SymbolStruct;

public class SpecialSymbolCodeGenerator implements CodeGenerator<SymbolStruct<?>> {

	public static final SpecialSymbolCodeGenerator INSTANCE = new SpecialSymbolCodeGenerator();

	/*
	 * Looks up the symbol in the :symbol-table and retrieves the local JVM
	 * variable number. It generates code to fetch that symbol and put it on
	 * the stack.
	 */

	@Override
	public void generate(final SymbolStruct<?> input, final IntermediateCodeGenerator codeGenerator) {
		final int theInt = EnvironmentAccessor.getSymbolAllocation(codeGenerator.bindingEnvironment, input);
		//****** this is a premature optimization. Good idea, but should wait for a new version of the compiler ****
		final int slot = 0;
		// it may not be in one of the accessible lambdas, so do it the old fashioned way
		if (slot > 0) {
			// now put the ALoad in the instruction stream
			codeGenerator.emitter.emitAload(slot);
			codeGenerator.emitter.emitCheckcast("lisp/common/type/Symbol");
		} else {
			codeGenerator.genCodeSpecialVariable(input);
		}
	}
}
