package jcl.compiler.old.functions;

import jcl.LispStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;

/**
 * Class Append - implements the standard Lisp function APPEND. There is a singleton
 * instance of this class that can be accessed via lisp.common.type.function.Append.FUNCTION. The
 * arguments correspond directly to the Common Lisp arguments.
 */
public class AppendFunction {

	public static final AppendFunction FUNCTION = new AppendFunction();

	/**
	 * Lisp Function - (APPEND list1 list2)
	 * Creates a shallow copy of the first list, sets the cdr of the last element
	 * of the new list to the second list2 and returns the complete new list.
	 * <p>
	 * NOTE: list2 can be any object or an improper list
	 * NOTE: list1 must be a proper list
	 *
	 * @return the new List
	 */
	@SuppressWarnings("unchecked")
	public LispStruct funcall(ListStruct list1, LispStruct appendingObjectOrList) {
		// is there a first list?
		if (list1 == NullStruct.INSTANCE) {
			return appendingObjectOrList;
		}

		if (appendingObjectOrList == NullStruct.INSTANCE) {
			return list1;
		}

		java.util.List<LispStruct> copyList = list1.getAsJavaList();
		copyList.add(appendingObjectOrList);

		if (appendingObjectOrList instanceof ConsStruct) {
			return ListStruct.buildProperList(copyList);
		} else {
			return ListStruct.buildDottedList(copyList);
		}
	}
}
