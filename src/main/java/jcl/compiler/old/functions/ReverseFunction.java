package jcl.compiler.old.functions;

import jcl.LispStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;

/**
 * The Reverse function reverses the order of sequence.
 * Reverse does not modify the argument sequence.
 * For Example:
 * <pre> (setq str "abc") =>  "abc"
 * (nreverse str) =>  "cba" </pre>
 */
public class ReverseFunction {

	/**
	 * Holds an instance of Reverse
	 */
	public static final ReverseFunction FUNCTION = new ReverseFunction();

	/**
	 * The funcall method creates a new sequence of the same type
	 * as sequence in reversed order
	 *
	 * @param arg1 a sequence
	 * @return a new sequence
	 */
	public LispStruct funcall(Object arg1) {
		ListStruct theList = (ListStruct) arg1;
		ListStruct newList = NullStruct.INSTANCE;
		while (theList != NullStruct.INSTANCE) {
			newList = (ListStruct) new ConsStruct(theList.getFirst(), newList);
			theList = theList.getRest();
		}
		return newList;
	}
}
