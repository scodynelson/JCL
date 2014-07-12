package jcl.compiler.old.functions;

import jcl.lists.ListStruct;

public class ValuesList {

	public static final ValuesList FUNCTION = new ValuesList();

	public Object[] funcall(Object args) {
		ListStruct asList = (ListStruct) args;
		return asList.getAsJavaList().toArray();
	}
}

