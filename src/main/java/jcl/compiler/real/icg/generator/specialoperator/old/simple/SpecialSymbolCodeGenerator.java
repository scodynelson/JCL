package jcl.compiler.real.icg.generator.specialoperator.old.simple;

import java.util.Optional;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.SymbolTable;
import jcl.compiler.real.environment.binding.SymbolEnvironmentBinding;
import jcl.compiler.real.environment.binding.SymbolLocalBinding;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SpecialSymbolCodeGenerator implements CodeGenerator<SymbolStruct<?>> {

	/*
	 * Looks up the symbol in the :symbol-table and retrieves the local JVM
	 * variable number. It generates code to fetch that symbol and put it on
	 * the stack.
	 */

	@Autowired
	private SpecialVariableCodeGenerator specialVariableCodeGenerator;

	@Override
	public void generate(final SymbolStruct<?> input, final JavaClassBuilder classBuilder) {
		final int theInt = getSymbolAllocation(classBuilder.getBindingEnvironment(), input);
		//****** this is a premature optimization. Good idea, but should wait for a new version of the compiler ****
		final int slot = 0;
		// it may not be in one of the accessible lambdas, so do it the old fashioned way
		if (slot > 0) {
			// now put the ALoad in the instruction stream
			classBuilder.getEmitter().emitAload(slot);
			classBuilder.getEmitter().emitCheckcast("lisp/common/type/Symbol");
		} else {
			specialVariableCodeGenerator.generate(input, classBuilder);
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
