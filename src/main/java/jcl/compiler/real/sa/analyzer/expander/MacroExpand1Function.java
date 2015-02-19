package jcl.compiler.real.sa.analyzer.expander;

import jcl.LispStruct;
import jcl.compiler.real.CompilerVariables;
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

	/**
	 * The funcall method expands the macro form call once. The environment is NIL.
	 *
	 * @param form The macro call.
	 * @return array[2] - The expansion form and generalized boolean.
	 * If the argument form is expanded returns true} else returns
	 * false.
	 */
	public MacroExpandReturn funcall(LispStruct form) {
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
	public MacroExpandReturn funcall(LispStruct arg1, LispStruct env) {
		MacroExpandReturn macroExpandReturn;

		if (arg1 instanceof ListStruct) {
			ListStruct form = (ListStruct) arg1;
			//casting the form to a symbol to get ahold of the function slot to check
			//whether or not it is of type MacroFunction
			if (form.getFirst() instanceof SymbolStruct) {
				// there may be a macroLet in here
				SymbolStruct<?> theMacroName = (SymbolStruct) form.getFirst();
				//
				FunctionStruct expander = theMacroName.getFunction();

				if (expander instanceof MacroFunctionExpander) {
					//found a macro function, now casting it to a macro function
					// get the macroexpand-hook
					FunctionStruct theHookFn = CompilerVariables.MACROEXPAND_HOOK.getValue();
					LispStruct hookResult = theHookFn.apply(expander, form, env);
					return new MacroExpandReturn(hookResult, true);
				}
			}
		} else if (arg1 instanceof SymbolStruct) {
			// this is where we will check for SYMBOL-MACRO symbol expansion
		}
		return new MacroExpandReturn(arg1, false);
	}
}
