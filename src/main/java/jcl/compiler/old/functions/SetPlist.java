package jcl.compiler.old.functions;

import jcl.LispStruct;
import jcl.lists.ConsStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;

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
		} else if (indicator.equals(place.getFirst())) {
			// found it, change the value
			final ListStruct rest = place.getRest();
			rest.setElement(0, newValue);
			return true;
		} else {
			return setHelper(place.getRest().getRest(), indicator, newValue);
		}
	}
}
