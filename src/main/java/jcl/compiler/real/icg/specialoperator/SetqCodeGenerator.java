package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.binding.SymbolLocalBinding;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.SpecialSymbolCodeGenerator;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SymbolStruct;

import java.util.Optional;

public class SetqCodeGenerator implements CodeGenerator<ListStruct> {

	public static final SetqCodeGenerator INSTANCE = new SetqCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator) {

		ListStruct restOfList = input.getRest();
		while (!NullStruct.INSTANCE.equals(restOfList)) {
			// get the first symbol
			final SymbolStruct<?> symbol = (SymbolStruct) restOfList.getFirst();
			// step over the variable
			restOfList = restOfList.getRest();
			// get the form to evaluate
			codeGenerator.icgMainLoop(restOfList.getFirst());
			// value is now on the stack, we have to determine where to put it
			// determine if this is a local variable or a special variable
			final LexicalEnvironment binding = EnvironmentAccessor.getBindingEnvironment(codeGenerator.bindingEnvironment, symbol, true);
			if (!binding.equals(LexicalEnvironment.NULL)
					&& (getSymbolScope(codeGenerator.bindingEnvironment, symbol) != Scope.DYNAMIC)) {
				// so find what local slot it is
				final int slot = IntermediateCodeGenerator.genLocalSlot(symbol, binding); // drop the %let
				// if this is the last set, dup the value so it's returned
				if (restOfList.getRest().equals(NullStruct.INSTANCE)) {
					codeGenerator.emitter.emitDup(); // leaves the value on the stack
				}
				codeGenerator.emitter.emitAstore(slot);
			} else {
				// now the value is on the stack, is the variable local or special?
				SpecialSymbolCodeGenerator.INSTANCE.generate(symbol, codeGenerator);
				codeGenerator.emitter.emitSwap();
				codeGenerator.emitter.emitInvokeinterface("lisp/common/type/Symbol", "setValue", "(Ljava/lang/Object;)", "Ljava/lang/Object;", true);
				if (!restOfList.getRest().equals(NullStruct.INSTANCE)) {
					codeGenerator.emitter.emitPop(); // pop the value on the stack execpt for the last one
				}
			}
			// step through the rest pair or done
			restOfList = restOfList.getRest();
		}
	}

	private static Scope getSymbolScope(final LexicalEnvironment currentEnvironment, final SymbolStruct<?> variable) {

		// look up the symbol in the symbol table
		final Optional<SymbolLocalBinding> symPList = EnvironmentAccessor.getSymbolTableEntry(currentEnvironment, variable);
		return symPList.get().getScope();
	}
}
