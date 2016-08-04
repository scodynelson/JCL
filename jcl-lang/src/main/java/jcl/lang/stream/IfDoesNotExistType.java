/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.SymbolStructImpl;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.NILStruct;

/**
 * Created by codynelson on 2/15/16.
 */
public enum IfDoesNotExistType {

	ERROR(CommonLispSymbols.ERROR_KEYWORD),
	CREATE(CommonLispSymbols.CREATE_KEYWORD),
	NIL(NILStruct.INSTANCE);

	private final SymbolStructImpl keyword;

	IfDoesNotExistType(final SymbolStructImpl keyword) {
		this.keyword = keyword;
	}

	public static IfDoesNotExistType fromValue(final SymbolStructImpl keyword) {
		for (IfDoesNotExistType ifDoesNotExistType : values()) {
			if (ifDoesNotExistType.keyword.equals(keyword)) {
				return ifDoesNotExistType;
			}
		}
		throw new TypeErrorException("Unknown :if-does-not-exist type.");
	}
}
