package jcl.compiler.real.icg;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.SymbolTable;
import jcl.compiler.real.environment.binding.SymbolEnvironmentBinding;
import jcl.compiler.real.environment.binding.SymbolLocalBinding;
import jcl.symbols.SymbolStruct;

import java.util.Optional;

public class SpecialSymbolCodeGenerator implements CodeGenerator<SymbolStruct<?>> {

	public static final SpecialSymbolCodeGenerator INSTANCE = new SpecialSymbolCodeGenerator();

	/*
	 * Looks up the symbol in the :symbol-table and retrieves the local JVM
	 * variable number. It generates code to fetch that symbol and put it on
	 * the stack.
	 */

	@Override
	public void generate(final SymbolStruct<?> input, final IntermediateCodeGenerator codeGenerator) {
		final int theInt = getSymbolAllocation(codeGenerator.bindingEnvironment, input);
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

	private static int getSymbolAllocation(final Environment currentEnvironment, final SymbolStruct<?> variable) {

		// look up the symbol in the symbol table
		final Optional<SymbolLocalBinding> symPList = getSymbolTableEntry(currentEnvironment, variable);
//        List symPList = getSymbolInTable(currentEnvironmentInner, variable);

//        // (:ALLOCATION (:LOCAL . n) :BINDING :FREE :SCOPE :DYNAMIC :TYPE T)
//        // we need the local slot in the allocation, get the CDR of the GET of :ALLOCATION

//        // if the cons starts with LOCAL, we're there
//        // otherwise, we have to go to the actual env of allocation
//        if (alloc.getFirst() != KeywordOld.Local) {
//            symPList = getSymbolInTable(alloc, variable);
		if (symPList.isPresent()) {
			return symPList.get().getAllocation().getPosition();
		} else {
			return -1;
		}
	}

	private static Optional<SymbolLocalBinding> getSymbolTableEntry(final Environment currentEnvironment,
	                                                               final SymbolStruct<?> variable) {

		// look up the symbol in the symbol table
		final SymbolTable symTable = currentEnvironment.getSymbolTable();

		Optional<SymbolLocalBinding> symbolLocalBinding = symTable.getDynamicLocalBinding(variable);

		// if the cons starts with LOCAL, we're there
		// otherwise, we have to go to the actual env of allocation
		if (!symbolLocalBinding.isPresent()) {
			final Optional<SymbolEnvironmentBinding> symbolEnvironmentBinding = symTable.getDynamicEnvironmentBinding(variable);
			if (symbolEnvironmentBinding.isPresent()) {
				final SymbolEnvironmentBinding realSEB = symbolEnvironmentBinding.get();
				final SymbolTable sebSymbolTable = realSEB.getBinding().getSymbolTable();

				symbolLocalBinding = sebSymbolTable.getDynamicLocalBinding(variable);
			}
		}

		return symbolLocalBinding;
	}
}
