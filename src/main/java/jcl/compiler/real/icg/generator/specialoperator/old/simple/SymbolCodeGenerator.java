package jcl.compiler.real.icg.generator.specialoperator.old.simple;

import java.util.Optional;

import jcl.compiler.real.environment.BindingEnvironment;
import jcl.compiler.real.environment.Closure;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.InnerFunctionEnvironment;
import jcl.compiler.real.environment.SymbolTable;
import jcl.compiler.real.environment.allocation.ClosureAllocation;
import jcl.compiler.real.environment.allocation.PositionAllocation;
import jcl.compiler.real.environment.binding.Binding;
import jcl.compiler.real.environment.binding.ClosureBinding;
import jcl.compiler.real.environment.binding.SymbolClosureBinding;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SymbolCodeGenerator implements CodeGenerator<SymbolStruct<?>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(SymbolCodeGenerator.class);

	@Autowired
	private SpecialSymbolCodeGenerator specialSymbolCodeGenerator;

	@Override
	public void generate(final SymbolStruct<?> input, final JavaClassBuilder classBuilder) {
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
		final Closure closure = classBuilder.getBindingEnvironment().getClosure();
		final Optional<ClosureBinding> closureBinding = closure.getBinding(input);
		if (closureBinding.isPresent()) {
			// #1. it's in a local closure
			// get the position in the closure
			final int position = closureBinding.get().getPosition();
			// now get the object out of the current closure
			// get this
			classBuilder.getEmitter().emitAload(0);
			classBuilder.getEmitter().emitInvokespecial("lisp/common/function/FunctionBaseClass", "getClosure", "()", "Llisp/extensions/type/Closure;", false);
			classBuilder.getEmitter().emitLdc(position);
			classBuilder.getEmitter().emitLdc(0);
			classBuilder.getEmitter().emitInvokeinterface("lisp/extensions/type/Closure", "getBindingAt", "(II)", "Ljava/lang/Object;", true);
		} else {
			// set up for 2 or 3
			final SymbolTable symbolTable = classBuilder.getBindingEnvironment().getSymbolTable();
			final boolean hasSymbolTableEntry = symbolTable.hasBinding(input);
			// (:allocation ... :
			if (hasSymbolTableEntry) {
				// it's 2 or 3
				// check the scope, if :dynamic it's 3

				final boolean hasDynamicBinding = symbolTable.hasDynamicBinding(input);
				final boolean hasClosureBinding = symbolTable.hasClosureBinding(input);
				if (hasDynamicBinding) {
					// it's number 3
					specialSymbolCodeGenerator.generate(input, classBuilder);
					classBuilder.getEmitter().emitInvokestatic("jcl/symbols/SymbolStruct", "getValue", "()", "Ljava/lang/Object;", true);
				} else if (hasClosureBinding) {
					final SymbolClosureBinding entry = symbolTable.getClosureBinding(input).get();
					// it's door number 2
					// get the allocation parameter
					final ClosureAllocation closureAllocation = entry.getAllocation();
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
					classBuilder.getEmitter().emitAload(0);
					// get the current closure
					classBuilder.getEmitter().emitInvokespecial("lisp/common/function/FunctionBaseClass", "getClosure", "()", "Llisp/extensions/type/Closure;", false);
					// set up the constants for seeking
					classBuilder.getEmitter().emitLdc(position);
					classBuilder.getEmitter().emitLdc(nesting);
					// now give chase up the chain
					classBuilder.getEmitter().emitInvokeinterface("lisp/extensions/type/Closure", "getBindingAt", "(II)", "Ljava/lang/Object;", true);
				} else {
					// go find it
					final Environment binding = getBindingEnvironment(classBuilder.getBindingEnvironment(), input);

					// get the :bindings list
					// ((x :allocation ...) (y :allocation ...) ...)
					final Binding<?> symBinding = binding.getLexicalBinding(input).get();
					// (:allocation ... :scope ... )
					// get the allocated slot for the symbol and put it on the stack
					final int slot = ((PositionAllocation) symBinding.getAllocation()).getPosition();

					classBuilder.getEmitter().emitAload(slot);
				}
			} else {
				// it's 4
				final Environment binding = getBindingEnvironment(classBuilder.getBindingEnvironment(), input);
				if (binding.equals(Environment.NULL)) {
					// This is a truly free variable, check to make sure it's special
					// if not, issue a warning, then treat it as special
//					if (!input.isSpecial()) {
//						LOGGER.warn("; Warning: variable {} is assumed free", input);
//					}
					specialSymbolCodeGenerator.generate(input, classBuilder);
					classBuilder.getEmitter().emitInvokestatic("jcl/symbols/SymbolStruct", "getValue", "()", "Ljava/lang/Object;", true);
				} else {
					// get the :bindings list
					// ((x :allocation ...) (y :allocation ...) ...)
					final Binding<?> symBinding = binding.getLexicalBinding(input).get();
					// (:allocation ... :scope ... )
					// get the allocated slot for the symbol and put it on the stack
					final int slot = ((PositionAllocation) symBinding.getAllocation()).getPosition();

					classBuilder.getEmitter().emitAload(slot);
				}
			}
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
