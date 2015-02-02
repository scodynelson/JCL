package jcl.compiler.real.icg;

import jcl.compiler.real.environment.Allocation;
import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.ClosureAllocation;
import jcl.compiler.real.environment.ClosureBinding;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.compiler.real.environment.Scope;
import jcl.compiler.real.environment.SymbolBinding;
import jcl.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Optional;

public class SymbolCodeGenerator implements CodeGenerator<SymbolStruct<?>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(SymbolCodeGenerator.class);

	public static final SymbolCodeGenerator INSTANCE = new SymbolCodeGenerator();

	@Override
	public void generate(final SymbolStruct<?> input, final IntermediateCodeGenerator codeGenerator) {
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
		final Closure closure = codeGenerator.bindingEnvironment.getEnvironmentClosure();
		final Optional<ClosureBinding> closureBinding = closure.getBinding(input);
		if (!closureBinding.isPresent()) {
			// set up for 2 or 3
			final Optional<SymbolBinding> entryOptional = codeGenerator.bindingEnvironment.getSymbolTable().getBinding(input);
			// (:allocation ... :
			if (!entryOptional.isPresent()) {
				// it's 4
				final Environment binding = EnvironmentAccessor.getBindingEnvironment(codeGenerator.bindingEnvironment, input, true);
				if (binding.equals(Environment.NULL)) {
					// This is a truly free variable, check to make sure it's special
					// if not, issue a warning, then treat it as special
					if (!input.isSpecial()) {
						LOGGER.warn("; Warning: variable {} is assumed free", input);
					}
					SpecialSymbolCodeGenerator.INSTANCE.generate(input, codeGenerator);
					codeGenerator.emitter.emitInvokestatic("jcl/symbols/SymbolStruct", "getValue", "()", "Ljava/lang/Object;", true);
				} else {
					final int slot = IntermediateCodeGenerator.genLocalSlot(input, binding);
					codeGenerator.emitter.emitAload(slot);
				}
			} else {
				// it's 2 or 3
				// check the scope, if :dynamic it's 3
				SymbolBinding entry = entryOptional.get();

				if (entry.getScope() == Scope.DYNAMIC) {
					// it's number 3
					SpecialSymbolCodeGenerator.INSTANCE.generate(input, codeGenerator);
					codeGenerator.emitter.emitInvokestatic("jcl/symbols/SymbolStruct", "getValue", "()", "Ljava/lang/Object;", true);
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
						final Optional<ClosureBinding> parentEntry = parentClosure.getBinding(input);
						// (x :position m :references n)
						final int position = parentEntry.get().getPosition();
						// get the current closure depth if any

						// have to find the first closure with a :depth in it. That's
						// the one that will be on the stack of the current lambda. The difference of
						// the 2 depths is the nesting level.
						final int closureDepth = closure.getDepth();
						final int nesting = closureDepth - parentDepth;

						// Whew!! Now we can gen some code
						// get this
						codeGenerator.emitter.emitAload(0);
						// get the current closure
						codeGenerator.emitter.emitInvokespecial("lisp/common/function/FunctionBaseClass", "getClosure", "()", "Llisp/extensions/type/Closure;", false);
						// set up the constants for seeking
						codeGenerator.emitter.emitLdc(position);
						codeGenerator.emitter.emitLdc(nesting);
						// now give chase up the chain
						codeGenerator.emitter.emitInvokeinterface("lisp/extensions/type/Closure", "getBindingAt", "(II)", "Ljava/lang/Object;", true);
					} else {
						// go find it
						final Environment binding = EnvironmentAccessor.getBindingEnvironment(codeGenerator.bindingEnvironment, input, true);
						final int slot = IntermediateCodeGenerator.genLocalSlot(input, binding);
						codeGenerator.emitter.emitAload(slot);
					}
				}
			}
		} else {
			// #1. it's in a local closure
			// get the position in the closure
			final int position = closureBinding.get().getPosition();
			// now get the object out of the current closure
			// get this
			codeGenerator.emitter.emitAload(0);
			codeGenerator.emitter.emitInvokespecial("lisp/common/function/FunctionBaseClass", "getClosure", "()", "Llisp/extensions/type/Closure;", false);
			codeGenerator.emitter.emitLdc(position);
			codeGenerator.emitter.emitLdc(0);
			codeGenerator.emitter.emitInvokeinterface("lisp/extensions/type/Closure", "getBindingAt", "(II)", "Ljava/lang/Object;", true);
		}
	}
}
