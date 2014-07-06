package jcl.compiler.old.functions;

import jcl.LispStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;

public final class GetPlist {

	private GetPlist() {
	}

	public static LispStruct funcall(final ListStruct arg1, final LispStruct arg2) {
		return funcall(arg1, arg2, NullStruct.INSTANCE);
	}

	public static LispStruct funcall(final ListStruct arg1, final LispStruct indicator, final LispStruct defaultValue) {

		if (arg1.equals(NullStruct.INSTANCE)) {
			return defaultValue;
		}

		if (indicator.equals(arg1.getFirst())) {
			return arg1.getRest().getFirst();
		}

		return funcall(arg1.getRest().getRest(), indicator, defaultValue);
	}
}
