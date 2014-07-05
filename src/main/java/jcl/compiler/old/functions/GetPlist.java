package jcl.compiler.old.functions;

import jcl.lists.ListStruct;
import jcl.lists.NullStruct;

/**
 * Lisp system function <CODE>%LOAD</CODE> for loading Lisp source files.
 * Unlike the Lisp <CODE>LOAD</CODE> function, this function only accepts
 * a filename, without the optional arguments.
 */
public class GetPlist {
	public static final GetPlist FUNCTION = new GetPlist();

	public Object funcall(Object arg1, Object arg2) {
		return funcall(arg1, arg2, NullStruct.INSTANCE);
	}

	public Object funcall(Object arg1, Object indicator, Object defaultValue) {
		ListStruct pList = (ListStruct) arg1;
		if (pList == NullStruct.INSTANCE) {
			return defaultValue;
		} else if (indicator == pList.getFirst()) {
			return pList.getRest().getFirst();
		} else {
			return funcall(pList.getRest().getRest(), indicator, defaultValue);
		}
	}

}
