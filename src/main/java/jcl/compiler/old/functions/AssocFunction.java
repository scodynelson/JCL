package jcl.compiler.old.functions;

import jcl.lists.ListStruct;
import jcl.lists.NullStruct;

/**
 * Returns the CAR of a list matching a provided key.  Takes a key as its
 * first argument and a list as its second argument.s
 */
public class AssocFunction {

	public static final AssocFunction FUNCTION = new AssocFunction();

	/**
	 * @param arg1 - The item to search for
	 * @param arg2 - The association list
	 * @return Object of type CONS
	 */
	public Object funcall(Object arg1, Object arg2) {
		ListStruct aList = (ListStruct) arg2;
		// now for the code
		if (aList == NullStruct.INSTANCE) {
			return NullStruct.INSTANCE;
		} else {
			ListStruct element = (ListStruct) aList.getFirst();
			if (arg1.equals(element.getFirst())) {
				return element;
			} else {
				return funcall(arg1, aList.getRest());
			}
		}
	}
}
