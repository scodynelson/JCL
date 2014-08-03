package jcl.compiler.old.functions;

import jcl.LispStruct;
import jcl.structs.lists.ConsStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;

public final class SetPlist {

	private SetPlist() {
	}

	public static ListStruct funcall(final ListStruct arg1, final LispStruct indicator, final LispStruct newValue) {
		if (setHelper(arg1, indicator, newValue)) {
			return arg1;
		} else {
			return new ConsStruct(indicator, new ConsStruct(newValue, arg1));
		}
	}

	private static boolean setHelper(final ListStruct place, final LispStruct indicator, final LispStruct newValue) {
		if (place.equals(NullStruct.INSTANCE)) {
			// didn't find it, tell the main fn that
			return false;
		}

		if (indicator.equals(place.getFirst())) {
			// found it, change the value
			final ListStruct rest = place.getRest();
			rest.setElement(0, newValue);
			return true;
		}

		return setHelper(place.getRest().getRest(), indicator, newValue);
	}
}
