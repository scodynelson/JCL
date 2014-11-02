package jcl.compiler.real.icg;

import jcl.compiler.old.EnvironmentAccessor;
import jcl.compiler.real.environment.Allocation;
import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.ClosureAllocation;
import jcl.compiler.real.environment.ClosureBinding;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.SymbolBinding;
import jcl.structs.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SymbolCodeGenerator {

	private static final Logger LOGGER = LoggerFactory.getLogger(SymbolCodeGenerator.class);

	public static void genCodeSymbolValue(final IntermediateCodeGenerator icg, final SymbolStruct<?> symbolStruct) {
		// must determine one of 4 options:
		// 1. this is in a closure that's local to the environment
		// => assoc on the closure property of the current env
		// 2. it is found in the symbol table and is a closure which must be found and accessed through the chain
		// => assoc on the symbol table property of the current environment
		// ==> allocation :closure, follow chain
		// 3. it is found in the symbol table and is a special variable. All work is done to the symbol itself
		// ==> free and dynamic
		// 4. the binding is completely local and allocated to a JVM local
		//    If there is no binding and this is special, it's really free!
		final Closure closure = icg.bindingEnvironment.getEnvironmentClosure();
		final ClosureBinding closureBinding = closure.getBinding(symbolStruct);
		if (closureBinding == null) {
			// set up for 2 or 3
			final SymbolBinding entry = EnvironmentAccessor.getSymbolInTable(icg.bindingEnvironment, symbolStruct);
			// (:allocation ... :
			if (entry == null) {
				// it's 4
				final Environment binding = EnvironmentAccessor.getBindingEnvironment(icg.bindingEnvironment, symbolStruct, true);
				if (binding.equals(Environment.NULL)) {
					// This is a truly free variable, check to make sure it's special
					// if not, issue a warning, then treat it as special
					if (!symbolStruct.isSpecial()) {
						LOGGER.warn("; Warning: variable {} is assumed free", symbolStruct);
					}
					genCodeSpecialSymbol(icg, symbolStruct);
					icg.emitter.emitInvokestatic("jcl/structs/symbols/SymbolStruct", "getValue", "()", "Ljava/lang/Object;", true);
				} else {
					final int slot = IntermediateCodeGenerator.genLocalSlot(symbolStruct, binding);
					icg.emitter.emitAload(slot);
				}
			} else {
				// it's 2 or 3
				// check the scope, if :dynamic it's 3
				if (entry.getScope() == Scope.DYNAMIC) {
					// it's number 3
					genCodeSpecialSymbol(icg, symbolStruct);
					icg.emitter.emitInvokestatic("jcl/structs/symbols/SymbolStruct", "getValue", "()", "Ljava/lang/Object;", true);
				} else {
					// it's door number 2
					// get the allocation parameter
					final Allocation allocation = entry.getAllocation();
					// may be a lexical binding up a few levels
					if (allocation instanceof ClosureAllocation) {
						final ClosureAllocation closureAllocation = (ClosureAllocation) allocation;
						// (:closure . #n#)
						// now we have the environment where the closure is defined
						// so pick it up, get the nesting depth and the position
						final Closure parentClosure = closureAllocation.getClosure();
						// (:closure (:depth . n) (x ...)...)
						final int parentDepth = parentClosure.getDepth();
						// (:depth . n) => n
						final ClosureBinding parentEntry = parentClosure.getBinding(symbolStruct);
						// (x :position m :references n)
						final int position = parentEntry.getPosition();
						// get the current closure depth if any

						// have to find the first closure with a :depth in it. That's
						// the one that will be on the stack of the current lambda. The difference of
						// the 2 depths is the nesting level.
						final int closureDepth = closure.getDepth();
						final int nesting = closureDepth - parentDepth;

						// Whew!! Now we can gen some code
						// get this
						icg.emitter.emitAload(0);
						// get the current closure
						icg.emitter.emitInvokespecial("lisp/common/function/FunctionBaseClass", "getClosure", "()", "Llisp/extensions/type/Closure;", false);
						// set up the constants for seeking
						icg.emitter.emitLdc(position);
						icg.emitter.emitLdc(nesting);
						// now give chase up the chain
						icg.emitter.emitInvokeinterface("lisp/extensions/type/Closure", "getBindingAt", "(II)", "Ljava/lang/Object;", true);
					} else {
						// go find it
						final Environment binding = EnvironmentAccessor.getBindingEnvironment(icg.bindingEnvironment, symbolStruct, true);
						final int slot = IntermediateCodeGenerator.genLocalSlot(symbolStruct, binding);
						icg.emitter.emitAload(slot);
					}
				}
			}
		} else {
			// #1. it's in a local closure
			// get the position in the closure
			final int position = closureBinding.getPosition();
			// now get the object out of the current closure
			// get this
			icg.emitter.emitAload(0);
			icg.emitter.emitInvokespecial("lisp/common/function/FunctionBaseClass", "getClosure", "()", "Llisp/extensions/type/Closure;", false);
			icg.emitter.emitLdc(position);
			icg.emitter.emitLdc(0);
			icg.emitter.emitInvokeinterface("lisp/extensions/type/Closure", "getBindingAt", "(II)", "Ljava/lang/Object;", true);
		}
	}

	/**
	 * Looks up the symbol in the :symbol-table and retrieves the local JVM
	 * variable number. It generates code to fetch that symbol and put it on
	 * the stack.
	 */
	public static void genCodeSpecialSymbol(final IntermediateCodeGenerator icg, final SymbolStruct<?> sym) {
		final int theInt = EnvironmentAccessor.getSymbolAllocation(icg.bindingEnvironment, sym);
		//****** this is a premature optimization. Good idea, but should wait for a new version of the compiler ****
		final int slot = 0;
		// it may not be in one of the accessible lambdas, so do it the old fashioned way
		if (slot > 0) {
			// now put the ALoad in the instruction stream
			icg.emitter.emitAload(slot);
			icg.emitter.emitCheckcast("lisp/common/type/Symbol");
		} else {
			icg.genCodeSpecialVariable(sym);
		}
	}
}
