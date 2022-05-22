/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.condition.exception.ReaderErrorException;

/**
 * Implements the illegal '#??" Lisp reader macros.
 */
public final class SharpIllegalReaderMacroFunction extends ReaderMacroFunctionImpl {

	public SharpIllegalReaderMacroFunction() {
		super("SHARP-ILLEGAL");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		throw new ReaderErrorException("Illegal sharp character " + codePoint);
	}
}
