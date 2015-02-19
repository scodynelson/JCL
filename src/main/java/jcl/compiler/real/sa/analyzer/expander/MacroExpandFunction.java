package jcl.compiler.real.sa.analyzer.expander;

import jcl.LispStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;

/**
 * Implementation of the Common Lisp function <CODE>MACROEXPAND</CODE>
 */
public class MacroExpandFunction {

	public static final MacroExpandFunction FUNCTION = new MacroExpandFunction();

	/**
	 * Single arg version of funcall
	 * Passes Arg1 to two Arg version of funcall
	 * Sets Arg2 to Null.Nill
	 */
	public MacroExpandReturn funcall(LispStruct arg1) {
		return funcall(arg1, NullStruct.INSTANCE);
	}

	/**
	 * Expands any macro calls in the arg1
	 * arg1 is a list that macroexpand will attempt to expand by recursively calling macroexpand1
	 * arg2 is the environment of the marco
	 */
	public MacroExpandReturn funcall(LispStruct arg1, LispStruct arg2) {
		if (arg1 instanceof ListStruct) {
			//keep expanding the original list with macroexpand1 until its second return value
			//is Null.NIL
			MacroExpandReturn expansion = MacroExpand1Function.FUNCTION.funcall(arg1, arg2);
			LispStruct expandedForm = expansion.getExpandedForm();

			boolean wasExpanded = expansion.wasExpanded();

			// Now keep going until the form doesn't change
			while (wasExpanded) {
				expansion = funcall(expandedForm, arg2);
				expandedForm = expansion.getExpandedForm();
				wasExpanded = expansion.wasExpanded();
			}
			return expansion;
		} else {
			return new MacroExpandReturn(arg1, false);
		}
	}
}
