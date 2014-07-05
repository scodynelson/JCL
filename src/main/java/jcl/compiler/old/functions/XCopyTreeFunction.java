package jcl.compiler.old.functions;

import jcl.LispStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;

/**
 * Implements the Common Lisp function COPY-TREE.
 * Returns a new tree with the same shape and leaves as the provided tree, but in which
 * all the tree structure consists of new CONS. If an atom is provided,
 * returns the atom.
 */
public class XCopyTreeFunction {

	/**
	 * Holds an instance of CopyTree
	 */
	public static final XCopyTreeFunction FUNCTION = new XCopyTreeFunction();

	/**
	 * The funcall method returns a copy of the orginal list.
	 *
	 * @param arg1 A tree.
	 * @return A copy of the orginal tree
	 */
	@SuppressWarnings("unchecked")
	public LispStruct funcall(LispStruct arg1) {
		if (arg1 instanceof ConsStruct) {
			ListStruct orgLst = (ListStruct) arg1;
			ListStruct cLst = orgLst;
			// checks for an empty list
			if (orgLst == NullStruct.INSTANCE) {
				return orgLst;
			} else {
	            /* loop through the orginial list's elements and create a new list of the elements */
				cLst = new ConsStruct(FUNCTION.funcall(orgLst.getFirst()), FUNCTION.funcall(((ConsStruct) arg1).getCdr()));
				// return the copied list
				return cLst;
			}
		} else {
			return arg1;
		}
	}
}

