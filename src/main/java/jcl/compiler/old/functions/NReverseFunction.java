package jcl.compiler.old.functions;

import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;

/**
 * The NReverse function reverses the order of a sequence.
 * The function might either create a new sequence, modify the argument sequence, or both.
 * For Example:
 * <pre> (setq str "abc") =>  "abc"
 * (nreverse str) =>  "cba" </pre>
 */
public class NReverseFunction {

	/**
	 * Holds an instance of NReverse
	 */
	public static final NReverseFunction FUNCTION = new NReverseFunction();

	@SuppressWarnings("unchecked")
	public Object funcall(Object arg1) {
		ListStruct list = (ListStruct) arg1;
		if (list == NullStruct.INSTANCE) {
			return list;
		} else {
			ListStruct prevCons = NullStruct.INSTANCE;
			ListStruct cons = list;
			ListStruct nxtCons = list.getRest();
			while (nxtCons != NullStruct.INSTANCE) {
				((ConsStruct) cons).setCdr(prevCons);
				prevCons = cons;
				cons = nxtCons;
				nxtCons = nxtCons.getRest();
			}
			((ConsStruct) cons).setCdr(prevCons);
			return cons;
		}
	}
}
