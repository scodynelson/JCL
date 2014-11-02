package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.SymbolCodeGenerator;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.symbols.SymbolStruct;

public class SetqCodeGenerator {

	public static void genCodeSetq(final IntermediateCodeGenerator icg, ListStruct list) {
		list = list.getRest();
		while (!NullStruct.INSTANCE.equals(list)) {
			// get the first symbol
			final SymbolStruct<?> symbol = (SymbolStruct) list.getFirst();
			// step over the variable
			list = list.getRest();
			// get the form to evaluate
			icg.icgMainLoop(list.getFirst());
			// value is now on the stack, we have to determine where to put it
			// determine if this is a local variable or a special variable
			final Environment binding = EnvironmentAccessor.getBindingEnvironment(icg.bindingEnvironment, symbol, true);
			if (!binding.equals(Environment.NULL)
					&& (EnvironmentAccessor.getSymbolScope(icg.bindingEnvironment, symbol) != Scope.DYNAMIC)) {
				// so find what local slot it is
				final int slot = IntermediateCodeGenerator.genLocalSlot(symbol, binding); // drop the %let
				// if this is the last set, dup the value so it's returned
				if (list.getRest().equals(NullStruct.INSTANCE)) {
					icg.emitter.emitDup(); // leaves the value on the stack
				}
				icg.emitter.emitAstore(slot);
			} else {
				// now the value is on the stack, is the variable local or special?
				SymbolCodeGenerator.genCodeSpecialSymbol(icg, symbol);
				icg.emitter.emitSwap();
				icg.emitter.emitInvokeinterface("lisp/common/type/Symbol", "setValue", "(Ljava/lang/Object;)", "Ljava/lang/Object;", true);
				if (!list.getRest().equals(NullStruct.INSTANCE)) {
					icg.emitter.emitPop(); // pop the value on the stack execpt for the last one
				}
			}
			// step through the rest pair or done
			list = list.getRest();
		}
	}
}
