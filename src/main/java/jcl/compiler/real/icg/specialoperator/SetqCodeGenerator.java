package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.environment.BindingEnvironment;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.InnerFunctionEnvironment;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.SpecialSymbolCodeGenerator;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SymbolStruct;

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
			final Environment binding = getBindingEnvironment(codeGenerator.bindingEnvironment, symbol);
			final boolean hasDynamicBinding = codeGenerator.bindingEnvironment.getSymbolTable().hasDynamicBinding(symbol);
			if (binding.equals(Environment.NULL) || hasDynamicBinding) {
				// now the value is on the stack, is the variable local or special?
				SpecialSymbolCodeGenerator.INSTANCE.generate(symbol, codeGenerator);
				codeGenerator.emitter.emitSwap();
				codeGenerator.emitter.emitInvokeinterface("lisp/common/type/Symbol", "setValue", "(Ljava/lang/Object;)", "Ljava/lang/Object;", true);
				if (!restOfList.getRest().equals(NullStruct.INSTANCE)) {
					codeGenerator.emitter.emitPop(); // pop the value on the stack execpt for the last one
				}
			} else {
				// so find what local slot it is
				final int slot = IntermediateCodeGenerator.genLocalSlot(symbol, binding); // drop the %let
				// if this is the last set, dup the value so it's returned
				if (restOfList.getRest().equals(NullStruct.INSTANCE)) {
					codeGenerator.emitter.emitDup(); // leaves the value on the stack
				}
				codeGenerator.emitter.emitAstore(slot);
			}
			// step through the rest pair or done
			restOfList = restOfList.getRest();
		}
	}

	private static Environment getBindingEnvironment(final Environment environment, final SymbolStruct<?> variable) {

		Environment currentEnvironment = environment;

		while (!currentEnvironment.equals(Environment.NULL)) {

			if (currentEnvironment.hasLexicalBinding(variable)) {

				if ((currentEnvironment instanceof BindingEnvironment) && !(currentEnvironment instanceof InnerFunctionEnvironment)) {
					return currentEnvironment;
				}
			}

			currentEnvironment = currentEnvironment.getParent();
		}

		return currentEnvironment;
	}
}
