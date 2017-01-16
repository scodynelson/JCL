/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.StringStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.statics.ReaderVariables;
import jcl.reader.Reader;
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

	@Autowired
	public SharpPReaderMacroFunction(final Reader reader) {
		super("SHARP-P");
		this.reader = reader;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_P, this);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_P, this);
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_P) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_P);

		final LispStruct token = reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NILStruct.INSTANCE;
		}

		if (token instanceof StringStruct) {
			final StringStruct pathnameString = (StringStruct) token;
			final String namestring = pathnameString.toJavaString();
			return LispStructFactory.toPathname(namestring);
		} else {
			throw new ReaderErrorException("The value " + token + " is not of expected type STRING in argument to #P.");
		}
	}
}
