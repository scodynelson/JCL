package jcl.compiler.old.functions;

import jcl.LispStruct;
import jcl.compiler.old.expander.MacroFunctionExpander;
import jcl.compiler.old.symbol.VariableOld;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SymbolStruct;

/**
 * Implements the Common Lisp function Macroex[and-1.
 * The MacroExpand1 function expands a form if it is of type MacroFunction.
 * If form is a macro form, then macroexpand-1 expands the macro form call once.
 */
public class MacroExpand1Function {
	/**
	 * Holds an instance of MacroExpand1
	 */
	public static final MacroExpand1Function FUNCTION = new MacroExpand1Function();

	/** Holds the basic function for *MACROEXPAND-HOOK* */
	static {
		VariableOld.MacroexpandHook.setValue(BaseMacroExpandFn.FUNCTION);
	}

	/**
	 * The funcall method expands the macro form call once. The environment is NIL.
	 *
	 * @param form The macro call.
	 * @return array[2] - The expansion form and generalized boolean.
	 * If the argument form is expanded returns true} else returns
	 * false.
	 */
	public Object funcall(Object form) {
		return funcall(form, NullStruct.INSTANCE);
	}

	/**
	 * The funcall method expands the macro form call once.
	 *
	 * @param arg1 A form. This is a macro call.
	 * @param env  An environment object.
	 * @return array[2] - The expansion form and generalized boolean.
	 * If the argument form is expanded returns true} else returns
	 * false.
	 */
	@SuppressWarnings("unchecked")
	public Object funcall(Object arg1, LispStruct env) {
		Object[] rtnValues = new Object[2];

		if (arg1 instanceof ListStruct) {
			ListStruct form = (ListStruct) arg1;
			//casting the form to a symbol to get ahold of the function slot to check
			//whether or not it is of type MacroFunction
			if (form.getFirst() instanceof SymbolStruct) {
				// there may be a macroLet in here
				SymbolStruct theMacroName = (SymbolStruct) form.getFirst();
				//
				FunctionStruct expander = theMacroName.getFunction();

				if (expander instanceof MacroFunctionExpander) {
					//found a macro function, now casting it to a macro function
					// get the macroexpand-hook
					FunctionStruct theHookFn = (FunctionStruct) VariableOld.MacroexpandHook.getValue();
					rtnValues[0] = theHookFn.apply(expander, form, env);
					rtnValues[1] = true;
					return rtnValues;
				}
			}
		} else if (arg1 instanceof SymbolStruct) {
			// this is where we will check for SYMBOL-MACRO symbol expansion
		}
		rtnValues[0] = arg1;
		rtnValues[1] = false;
		return rtnValues;
	}

	// Now we make the base function for *macroexpand-hook*
	public static class BaseMacroExpandFn extends FunctionStruct {

		public static final BaseMacroExpandFn FUNCTION = new BaseMacroExpandFn();

		static {
			VariableOld.MacroexpandHook.setValue(FUNCTION);
		}

		@Override
		public LispStruct apply(LispStruct... lispStructs) {
			return funcall((MacroFunctionExpander) lispStructs[0], (ListStruct) lispStructs[1], lispStructs[2]);
		}

		public LispStruct funcall(MacroFunctionExpander expander, ListStruct form, LispStruct env) {
			return expander.expand(form, env);
		}
	}
}
