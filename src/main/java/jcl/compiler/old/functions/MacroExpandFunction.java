package jcl.compiler.old.functions;

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
	public Object funcall(Object arg1) {
		return funcall(arg1, NullStruct.INSTANCE);
	}

	/**
	 * Expands any macro calls in the arg1
	 * arg1 is a list that macroexpand will attempt to expand by recursively calling macroexpand1
	 * arg2 is the environment of the marco
	 */
	@SuppressWarnings("unchecked")
	public Object funcall(Object arg1, LispStruct arg2) {
		if (arg1 instanceof ListStruct) {
			//keep expanding the original list with macroexpand1 until its second return value
			//is Null.NIL
			Object[] expansion = (Object[]) MacroExpand1Function.FUNCTION.funcall(arg1, arg2);
			Object expandedForm = expansion[0];
			Boolean macroExpandResult = (Boolean) expansion[1];

			// Now keep going until the form doesn't change
			while (expansion[1] == Boolean.TRUE) {
				expansion = (Object[]) FUNCTION.funcall(expandedForm, arg2);
				expandedForm = expansion[0];
			}
			expansion[1] = macroExpandResult;
			return expansion;
		} else {
			Object[] retValues = new Object[2];
			retValues[0] = arg1;
			retValues[1] = false;
			return retValues;
		}
	}
}
