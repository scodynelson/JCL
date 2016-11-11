/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.NILStruct;

/**
 * Created by codynelson on 2/15/16.
 */
public enum IfExistsType {

	ERROR(CommonLispSymbols.ERROR_KEYWORD),
	NEW_VERSION(CommonLispSymbols.NEW_VERSION_KEYWORD),
	RENAME(CommonLispSymbols.RENAME_KEYWORD),
	RENAME_AND_DELETE(CommonLispSymbols.RENAME_AND_DELETE_KEYWORD),
	OVERWRITE(CommonLispSymbols.OVERWRITE_KEYWORD),
	APPEND(CommonLispSymbols.APPEND_KEYWORD),
	SUPERSEDE(CommonLispSymbols.SUPERSEDE_KEYWORD),
	NIL(NILStruct.INSTANCE);

	private final SymbolStruct keyword;

	IfExistsType(final SymbolStruct keyword) {
		this.keyword = keyword;
	}

	public static IfExistsType fromValue(final SymbolStruct keyword) {
		for (IfExistsType ifExistsType : values()) {
			if (ifExistsType.keyword.equals(keyword)) {
				return ifExistsType;
			}
		}
		throw new TypeErrorException("Unknown :if-exists type.");
	}
}