/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import java.util.List;

public final class ValuesStructs {

	public static void addValuesToList(final List<LispStruct> lispStructs, final LispStruct lispStruct) {
		if (lispStruct instanceof ValuesStruct) {
			final List<LispStruct> valuesList = ((ValuesStruct) lispStruct).getValuesList();
			lispStructs.addAll(valuesList);
		} else {
			lispStructs.add(lispStruct);
		}
	}

	public static LispStruct extractPrimaryValue(final LispStruct lispStruct) {
		if (lispStruct instanceof ValuesStruct) {
			return ((ValuesStruct) lispStruct).getPrimaryValue();
		}
		return lispStruct;
	}
}
