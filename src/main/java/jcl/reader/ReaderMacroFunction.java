/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import java.math.BigInteger;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.functions.FunctionStruct;
import jcl.numbers.IntegerStruct;
import jcl.streams.InputStream;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;

/**
 * Abstract implementation definition for all Reader defined macro functions that read character macros based off of a
 * provided {@link Integer} code point.
 */
public abstract class ReaderMacroFunction extends FunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -5244042303586458372L;

	@Autowired
	private ApplicationContext applicationContext;

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {

		final InputStream stream = (InputStream) lispStructs[0];

		final CharacterStruct macroCharacter = (CharacterStruct) lispStructs[1];
		final int codePoint = macroCharacter.getCodePoint();

		// TODO: DD-anomaly
		final BigInteger numberArgument;
		if (isDispatch()) {
			final IntegerStruct macroNumberArgument = (IntegerStruct) lispStructs[2];
			numberArgument = macroNumberArgument.getBigInteger();
		} else {
			numberArgument = null;
		}

		final Reader reader = applicationContext.getBean(Reader.class, stream);

		return readMacro(codePoint, reader, numberArgument);
	}

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
	public abstract LispStruct readMacro(int codePoint, Reader reader, BigInteger numberArgument);

	/**
	 * Default method used to determine if the ReaderMacroFunction is a dispatching macro. The default value return is
	 * {@code #false}, however this is overridden in the internal dispatching table in a readtable.
	 *
	 * @return whether or not the ReaderMacroFunction is a dispatching macro
	 */
	public boolean isDispatch() {
		return false;
	}
}
