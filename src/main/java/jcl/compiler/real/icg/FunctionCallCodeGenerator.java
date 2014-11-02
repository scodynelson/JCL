package jcl.compiler.real.icg;

import jcl.structs.functions.FunctionStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.symbols.SymbolStruct;
import org.objectweb.asm.Label;

public class FunctionCallCodeGenerator {

	/**
	 * Function must already be on stack
	 * It selectively generates either a funcall or apply. The method prefers the funcall
	 * option when it can
	 * 1)determine that the function implements one of the FunctionN
	 * interfaces that is appropriate to the number of arguments, or
	 * 2) has access to the function class object and can determine if it supports
	 * a function method with the correct number of parameters, or
	 * 3) if it cannot determine either 1 or 2, it generates an apply.
	 */
	public static void genCodeFunctionCall(final IntermediateCodeGenerator icg, ListStruct list, final boolean acceptsMultipleValues) {
		// +1 -> fn
		final int argsExistCt = 0;
		SymbolStruct<?> theFnName = null;
		if (list.getFirst() instanceof SymbolStruct) {
			theFnName = (SymbolStruct) list.getFirst();
		}

		// drop leading function name or lambda
		list = list.getRest();
		final int numParams = list.size();
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
			icg.emitter.emitAload(1);  // put the list on the stack
			icg.emitter.emitCheckcast("lisp/common/type/ListStruct");
			//
			icg.emitter.emitInvokeinterface("lisp/common/type/Function", "checkArguments",
					"(Llisp/common/type/ListStruct;)", "Llisp/common/type/Boolean;", true);
			// throw away the result (it will throw an exception if something is wrong)
			icg.emitter.emitPop();
			// now the stack is where it was a little while ago
		}
		// +1 -> fn
		if (fnOk) {
			// Now evaluate the arguments. Puts all of them on the stack
			while (!list.equals(NullStruct.INSTANCE)) {
				icg.icgMainLoop(list.getFirst());
				// check for multiple value returns
				// maybe this returned multiple values
				if (!(icg.allowMultipleValues || acceptsMultipleValues)) {
					// call a short routine to handle a possible return,
					// leaving the value on top - except...
					// the routine messes with reg 0, you have to restore it
					// ... fnVal or fnVal[]
//                    emitter.emitJsr(mvfCheckStack.peek());
//                    // ... fnVal, this
//                    emitter.emitAstore(0);
					// ... fnVal
					final Label outLabel = new Label();
					icg.emitter.emitDup();
					icg.emitter.emitInstanceof("[Ljava/lang/Object;");
					icg.emitter.emitIfeq(outLabel);
					icg.emitter.emitCheckcast("[Ljava/lang/Object;");
					icg.emitter.emitLdc(0);
					icg.emitter.emitAaload();
					icg.emitter.visitMethodLabel(outLabel);
				}
				//TODO this isn't the best way to do this. Better if the compiler
				// knows all of the data flow.
				list = list.getRest();
			}
			// +numParams -> (fn), p, p, ...
			// if (numParams >= 0 || numParams <= 9) make funcall
			String paramsDesc = "(";
			for (int i = 0; i < numParams; i++) {
				paramsDesc += "Ljava/lang/Object;";
			}
			paramsDesc += ")";
//*******
			icg.emitter.emitInvokeinterface("lisp/extensions/type/Function" + numParams, "funcall", paramsDesc, "Ljava/lang/Object;", true);

			// +1 -> result
		} else {
			// apply
			// +1 -> fn
			icg.emitter.emitLdc(numParams);
			// +2 -> fn, numParams
			icg.emitter.emitAnewarray("java/lang/Object");
			// +2 -> fn, the array
			int count = 0;
			while (!list.equals(NullStruct.INSTANCE)) {
				icg.emitter.emitDup();
				// +3 -> fn, array, array
				icg.emitter.emitLdc(count);
				// +4 -> fn, array, array, index
				icg.icgMainLoop(list.getFirst());
				// check for multiple value returns
				//TODO this isn't the best way to do this. Better if the compiler
				// knows all of the data flow.
				// check for multiple value returns
				// maybe this returned multiple values
				if (!(icg.allowMultipleValues || acceptsMultipleValues)) {
					// call a short routine to handle a possible return,
					// leaving the value on top - except...
					// the routine messes with reg 0, you have to restore it
					// ... fnVal or fnVal[]
//                    emitter.emitJsr(mvaCheckStack.peek());
//                    // ... fnVal, this
//                    emitter.emitAstore(0);
					// ... fnVal
					final Label outLabel = new Label();
					icg.emitter.emitDup();
					icg.emitter.emitInstanceof("[Ljava/lang/Object;");
					icg.emitter.emitIfeq(outLabel);
					icg.emitter.emitCheckcast("[Ljava/lang/Object;");
					icg.emitter.emitLdc(0);
					icg.emitter.emitAaload();
					icg.emitter.visitMethodLabel(outLabel);
				}
				// +5 -> fn, array, array, index, value
				icg.emitter.emitAastore();
				// +2 -> fn, array
				list = list.getRest();
				count++;
			}
			// +2 -> fn, array
			icg.emitter.emitInvokestatic("lisp/common/type/List$Factory", "newInstance", "([Ljava/lang/Object;)", "Llisp/common/type/ListStruct;", false);
			// +2 -> fn, the list
			// Now if we have the list, if the compiler is set to safety > 0 - call checkArguments
			// Dup the list and the object on the stack
			if (false) { //(compilerSafety != 0) {
				// +2 -> fn, list
				// Dup the list and the object on the stack
				icg.emitter.emitDup2(); // we need the arg list to still be there to be there
				// +4 -> fn, list, fn, list
				icg.emitter.emitInvokeinterface("lisp/common/type/Function", "checkArguments", "(Llisp/common/type/ListStruct;)", "Llisp/common/type/Boolean;", true);
				// +3 -> fn, list, T
				// throw away the result (it will throw an exception if something is wrong)
				icg.emitter.emitPop();
				// +2 -> fn, list
				// now the stack is where it was a little while ago
			}
			icg.emitter.emitInvokeinterface("lisp/common/type/Function", "apply", "(Llisp/common/type/ListStruct;)", "Ljava/lang/Object;", true);
			// maybe this returned multiple values
			// +1 -> result
		}
	}
}
