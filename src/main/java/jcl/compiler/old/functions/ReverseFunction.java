package jcl.compiler.old.functions;

import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;

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
