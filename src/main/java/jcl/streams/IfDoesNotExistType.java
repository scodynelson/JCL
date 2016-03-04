/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import jcl.conditions.exceptions.TypeErrorException;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;

/**
 * Created by codynelson on 2/15/16.
 */
public enum IfDoesNotExistType {

	ERROR(CommonLispSymbols.ERROR_KEYWORD),
	CREATE(CommonLispSymbols.CREATE_KEYWORD),
	NIL(NILStruct.INSTANCE);

	private final SymbolStruct keyword;

	IfDoesNotExistType(final SymbolStruct keyword) {
		this.keyword = keyword;
	}

	public static IfDoesNotExistType fromValue(final SymbolStruct keyword) {
		for (IfDoesNotExistType ifDoesNotExistType : values()) {
			if (ifDoesNotExistType.keyword.equals(keyword)) {
				return ifDoesNotExistType;
			}
		}
		throw new TypeErrorException("Unknown :if-does-not-exist type.");
	}
}