/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct;

import java.util.List;

import jcl.LispStruct;

/**
 * Created by codynelson on 9/15/15.
 */
public final class ValuesStructs {

	public static void addValuesToList(final List<LispStruct> lispStructs, final LispStruct lispStruct) {
		if (lispStruct instanceof ValuesStruct) {
			final List<LispStruct> valuesList = ((ValuesStruct) lispStruct).getValuesList();
			lispStructs.addAll(valuesList);
		} else {
			lispStructs.add(lispStruct);
		}
	}
}
