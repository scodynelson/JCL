/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.Reader;

import java.math.BigInteger;

/**
 * Implements the illegal '#??" Lisp reader macros.
 */
public final class SharpIllegalReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpIllegalReaderMacroFunction INSTANCE = new SharpIllegalReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpIllegalReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		throw new ReaderErrorException("Illegal sharp character " + codePoint);
	}
}
