/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;

import jcl.functions.pathname.PathnameFunction;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.StringStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.readtable.Reader;
import jcl.lang.readtable.ReaderInputStreamStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

/**
 * Implements the '#p' Lisp reader macro.
 */
@Component
@DependsOn("readerBootstrap")
public class SharpPReaderMacroFunction extends ReaderMacroFunctionImpl {

	private final Reader reader;

	/**
	 * {@link Autowired} {@link PathnameFunction} used for getting a new {@link PathnameStruct} instance from the read
	 * in pathname namestring.
	 */
	private final PathnameFunction pathnameFunction;

	@Autowired
	public SharpPReaderMacroFunction(final Reader reader, final PathnameFunction pathnameFunction) {
		this.reader = reader;
		this.pathnameFunction = pathnameFunction;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_P, this);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_P, this);
	}

	@Override
	public LispStruct readMacro(final ReaderInputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_P) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_P);

		final LispStruct token = reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NILStruct.INSTANCE;
		}

		if (token instanceof StringStruct) {
			final StringStruct pathnameString = (StringStruct) token;
			return pathnameFunction.pathname(pathnameString);
		} else {
			throw new ReaderErrorException("The value " + token + " is not of expected type STRING in argument to #P.");
		}
	}
}
