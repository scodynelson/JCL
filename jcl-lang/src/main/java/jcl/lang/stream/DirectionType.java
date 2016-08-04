/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.SymbolStructImpl;
import jcl.lang.condition.exception.TypeErrorException;

/**
 * Created by codynelson on 2/15/16.
 */
public enum DirectionType {

	INPUT(CommonLispSymbols.INPUT_KEYWORD),
	OUTPUT(CommonLispSymbols.OUTPUT_KEYWORD),
	IO(CommonLispSymbols.IO_KEYWORD),
	PROBE(CommonLispSymbols.PROBE_KEYWORD);

	private final SymbolStructImpl keyword;

	DirectionType(final SymbolStructImpl keyword) {
		this.keyword = keyword;
	}

	public static DirectionType fromValue(final SymbolStructImpl keyword) {
		for (DirectionType directionType : values()) {
			if (directionType.keyword.equals(keyword)) {
				return directionType;
			}
		}
		throw new TypeErrorException("Unknown :direction type.");
	}
}
