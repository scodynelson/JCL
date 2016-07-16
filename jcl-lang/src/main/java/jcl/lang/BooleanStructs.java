/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

public final class BooleanStructs {

	public static BooleanStruct toLispBoolean(final Boolean aBoolean) {
		if (aBoolean == null) {
			return NILStruct.INSTANCE;
		}
		return aBoolean ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}
}
