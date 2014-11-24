package jcl.compiler.old.functions;

import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;

public final class ReverseFunction {

	private ReverseFunction() {
	}

	public static ListStruct funcall(final ListStruct arg1) {

		ListStruct theList = arg1;
		ListStruct newList = NullStruct.INSTANCE;
		while (!theList.equals(NullStruct.INSTANCE)) {
			newList = new ConsStruct(theList.getFirst(), newList);
			theList = theList.getRest();
		}
		return newList;
	}
}
