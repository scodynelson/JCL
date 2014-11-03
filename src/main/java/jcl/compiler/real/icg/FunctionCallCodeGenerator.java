package jcl.compiler.real.icg;

import jcl.structs.functions.FunctionStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.symbols.SymbolStruct;
import org.objectweb.asm.Label;

public class FunctionCallCodeGenerator implements CodeGenerator<ListStruct> {

	public static final FunctionCallCodeGenerator INSTANCE = new FunctionCallCodeGenerator();

	private boolean acceptsMultipleValues;

	/*
	 * Function must already be on stack
	 * It selectively generates either a funcall or apply. The method prefers the funcall
	 * option when it can
	 * 1)determine that the function implements one of the FunctionN
	 * interfaces that is appropriate to the number of arguments, or
	 * 2) has access to the function class object and can determine if it supports
	 * a function method with the correct number of parameters, or
	 * 3) if it cannot determine either 1 or 2, it generates an apply.
	 */

	@Override
	public void generate(ListStruct input, final IntermediateCodeGenerator codeGenerator) {
		// +1 -> fn
		final int argsExistCt = 0;
		SymbolStruct<?> theFnName = null;
		if (input.getFirst() instanceof SymbolStruct) {
			theFnName = (SymbolStruct) input.getFirst();
		}

		// drop leading function name or lambda
		input = input.getRest();
		final int numParams = input.size();
		// +2 -> fn, fn
		// +1 -> fn
		// still have fn on stack
		// make function call - done when it can prove that there's a function that
		// supports the FunctionN interface
		boolean fnOk = false;
		if (theFnName != null) {
			// get the interfaces of the fn
			final FunctionStruct theFn = theFnName.getFunction();
			if (theFn != null) {
				final String ifName = "lisp.extensions.type.Function" + numParams;
				final Class<?>[] interfaces = theFn.getClass().getInterfaces();
				for (final Class<?> anInterface : interfaces) {
					if (ifName.equals(anInterface.getName())) {
						fnOk = true;
						break;
					}
				}
			}
		}

		// add a call to checkArguments if the compiler safety is other than 0
		if (false) { //(compilerSafety != 0) {
			// Dup the list and the object on the stack
			codeGenerator.emitter.emitAload(1);  // put the list on the stack
			codeGenerator.emitter.emitCheckcast("lisp/common/type/ListStruct");
			//
			codeGenerator.emitter.emitInvokeinterface("lisp/common/type/Function", "checkArguments",
					"(Llisp/common/type/ListStruct;)", "Llisp/common/type/Boolean;", true);
			// throw away the result (it will throw an exception if something is wrong)
			codeGenerator.emitter.emitPop();
			// now the stack is where it was a little while ago
		}
		// +1 -> fn
		if (fnOk) {
			// Now evaluate the arguments. Puts all of them on the stack
			while (!input.equals(NullStruct.INSTANCE)) {
				codeGenerator.icgMainLoop(input.getFirst());
				// check for multiple value returns
				// maybe this returned multiple values
				if (!(codeGenerator.allowMultipleValues || acceptsMultipleValues)) {
					// call a short routine to handle a possible return,
					// leaving the value on top - except...
					// the routine messes with reg 0, you have to restore it
					// ... fnVal or fnVal[]
//                    emitter.emitJsr(mvfCheckStack.peek());
//                    // ... fnVal, this
//                    emitter.emitAstore(0);
					// ... fnVal
					final Label outLabel = new Label();
					codeGenerator.emitter.emitDup();
					codeGenerator.emitter.emitInstanceof("[Ljava/lang/Object;");
					codeGenerator.emitter.emitIfeq(outLabel);
					codeGenerator.emitter.emitCheckcast("[Ljava/lang/Object;");
					codeGenerator.emitter.emitLdc(0);
					codeGenerator.emitter.emitAaload();
					codeGenerator.emitter.visitMethodLabel(outLabel);
				}
				//TODO this isn't the best way to do this. Better if the compiler
				// knows all of the data flow.
				input = input.getRest();
			}
			// +numParams -> (fn), p, p, ...
			// if (numParams >= 0 || numParams <= 9) make funcall
			String paramsDesc = "(";
			for (int i = 0; i < numParams; i++) {
				paramsDesc += "Ljava/lang/Object;";
			}
			paramsDesc += ")";
//*******
			codeGenerator.emitter.emitInvokeinterface("lisp/extensions/type/Function" + numParams, "funcall", paramsDesc, "Ljava/lang/Object;", true);

			// +1 -> result
		} else {
			// apply
			// +1 -> fn
			codeGenerator.emitter.emitLdc(numParams);
			// +2 -> fn, numParams
			codeGenerator.emitter.emitAnewarray("java/lang/Object");
			// +2 -> fn, the array
			int count = 0;
			while (!input.equals(NullStruct.INSTANCE)) {
				codeGenerator.emitter.emitDup();
				// +3 -> fn, array, array
				codeGenerator.emitter.emitLdc(count);
				// +4 -> fn, array, array, index
				codeGenerator.icgMainLoop(input.getFirst());
				// check for multiple value returns
				//TODO this isn't the best way to do this. Better if the compiler
				// knows all of the data flow.
				// check for multiple value returns
				// maybe this returned multiple values
				if (!(codeGenerator.allowMultipleValues || acceptsMultipleValues)) {
					// call a short routine to handle a possible return,
					// leaving the value on top - except...
					// the routine messes with reg 0, you have to restore it
					// ... fnVal or fnVal[]
//                    emitter.emitJsr(mvaCheckStack.peek());
//                    // ... fnVal, this
//                    emitter.emitAstore(0);
					// ... fnVal
					final Label outLabel = new Label();
					codeGenerator.emitter.emitDup();
					codeGenerator.emitter.emitInstanceof("[Ljava/lang/Object;");
					codeGenerator.emitter.emitIfeq(outLabel);
					codeGenerator.emitter.emitCheckcast("[Ljava/lang/Object;");
					codeGenerator.emitter.emitLdc(0);
					codeGenerator.emitter.emitAaload();
					codeGenerator.emitter.visitMethodLabel(outLabel);
				}
				// +5 -> fn, array, array, index, value
				codeGenerator.emitter.emitAastore();
				// +2 -> fn, array
				input = input.getRest();
				count++;
			}
			// +2 -> fn, array
			codeGenerator.emitter.emitInvokestatic("lisp/common/type/List$Factory", "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/ListStruct;", false);
			// +2 -> fn, the list
			// Now if we have the list, if the compiler is set to safety > 0 - call checkArguments
			// Dup the list and the object on the stack
			if (false) { //(compilerSafety != 0) {
				// +2 -> fn, list
				// Dup the list and the object on the stack
				codeGenerator.emitter.emitDup2(); // we need the arg list to still be there to be there
				// +4 -> fn, list, fn, list
				codeGenerator.emitter.emitInvokeinterface("lisp/common/type/Function", "checkArguments", "(Llisp/common/type/ListStruct;)", "Llisp/common/type/Boolean;", true);
				// +3 -> fn, list, T
				// throw away the result (it will throw an exception if something is wrong)
				codeGenerator.emitter.emitPop();
				// +2 -> fn, list
				// now the stack is where it was a little while ago
			}
			codeGenerator.emitter.emitInvokeinterface("lisp/common/type/Function", "apply", "(Llisp/common/type/ListStruct;)", "Ljava/lang/Object;", true);
			// maybe this returned multiple values
			// +1 -> result
		}
	}

	public boolean isAcceptsMultipleValues() {
		return acceptsMultipleValues;
	}

	public void setAcceptsMultipleValues(final boolean acceptsMultipleValues) {
		this.acceptsMultipleValues = acceptsMultipleValues;
	}
}
