/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import java.io.Serializable;
import java.math.BigInteger;

import jcl.LispStruct;

/**
 * Interface definition for all Reader defined macro functions that read character macros based off of a provided
 * {@link Integer} code point.
 */
@FunctionalInterface
public interface ReaderMacroFunction extends Serializable {

	/**
	 * Serializable Version Unique Identifier.
	 */
	long serialVersionUID = -1L;

	/**
	 * Interpret the character stream from the provided {@link Reader} (up to End-of-File or new line) based on the
	 * provided {@code codePoint}.
	 *
	 * @param codePoint
	 * 		the character code point that determines the macro function
	 * @param reader
	 * 		the {@link Reader} used to read tokens
	 * @param numberArgument
	 * 		the optional number argument
	 *
	 * @return the parsed {@link LispStruct} token
	 */
	LispStruct readMacro(int codePoint, Reader reader, BigInteger numberArgument);

	/**
	 * Default method used to determine if the ReaderMacroFunction is a dispatching macro. The default value return is
	 * {@code #false}, however this is overridden in the internal dispatching table in a readtable.
	 *
	 * @return whether or not the ReaderMacroFunction is a dispatching macro
	 */
	default boolean isDispatch() {
		return false;
	}
}
