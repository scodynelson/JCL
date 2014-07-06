package jcl.compiler.old.functions;

import jcl.LispStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;

import java.util.List;

public final class AppendFunction {

	private AppendFunction() {
	}

	public static LispStruct funcall(final ListStruct arg1, final LispStruct arg2) {

		if (arg1.equals(NullStruct.INSTANCE)) {
			return arg2;
		}

		if (arg2.equals(NullStruct.INSTANCE)) {
			return arg1;
		}

		final List<LispStruct> javaList = arg1.getAsJavaList();
		javaList.add(arg2);

		if (arg2 instanceof ConsStruct) {
			return ListStruct.buildProperList(javaList);
		} else {
			return ListStruct.buildDottedList(javaList);
		}
	}
}
