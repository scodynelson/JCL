package jcl.compiler.old.functions;

import jcl.LispStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;

import java.util.List;

public final class AssocFunction {

	private AssocFunction() {
	}

	public static ListStruct funcall(final LispStruct arg1, final ListStruct arg2) {

		if (arg2.equals(NullStruct.INSTANCE)) {
			return arg2;
		}

		ListStruct returnObj = NullStruct.INSTANCE;

		final List<LispStruct> javaList = arg2.getAsJavaList();
		for (final LispStruct lispStruct : javaList) {
			final ListStruct element = (ListStruct) lispStruct;

			final LispStruct first = element.getFirst();
			if (arg1.equals(first)) {
				returnObj = element;
				break;
			}
		}

		return returnObj;
	}
}
