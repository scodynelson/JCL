/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;

/**
 * Created by codynelson on 2/15/16.
 */
public enum DirectionType {

	INPUT(CommonLispSymbols.INPUT_KEYWORD),
	OUTPUT(CommonLispSymbols.OUTPUT_KEYWORD),
	IO(CommonLispSymbols.IO_KEYWORD),
	PROBE(CommonLispSymbols.PROBE_KEYWORD);

	private final SymbolStruct keyword;

	DirectionType(final SymbolStruct keyword) {
		this.keyword = keyword;
	}

	public static DirectionType fromValue(final SymbolStruct keyword) {
		for (DirectionType directionType : values()) {
			if (directionType.keyword.eq(keyword)) {
				return directionType;
			}
		}
		throw new TypeErrorException("Unknown :direction type.");
	}
}
