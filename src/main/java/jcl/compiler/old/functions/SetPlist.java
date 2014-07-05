package jcl.compiler.old.functions;

import jcl.LispStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;

/**
 * Implements the Lisp function system::%set-plist. This is the function
 * used in the macro expansion of (setf (getf plist indicator....
 */
public class SetPlist {

	public static final SetPlist FUNCTION = new SetPlist();

	@SuppressWarnings("unchecked")
	public Object funcall(Object arg1, LispStruct indicator, LispStruct newValue) {
		ListStruct pList = (ListStruct) arg1;
		if (setHelper(pList, indicator, newValue)) {
			return pList;
		} else {
			return ConsFunction.FUNCTION.funcall(indicator, ConsFunction.FUNCTION.funcall(newValue, pList));
		}
	}

	@SuppressWarnings("unchecked")
	private boolean setHelper(ListStruct place, Object indicator, LispStruct newValue) {
		if (place == NullStruct.INSTANCE) {
			// didn't find it, tell the main fn that
			return false;
		} else if (indicator == place.getFirst()) {
			// found it, change the value
			ListStruct rest = place.getRest();
			rest.setElement(0, newValue);
			return true;
		} else {
			return setHelper(place.getRest().getRest(), indicator, newValue);
		}
	}
}
