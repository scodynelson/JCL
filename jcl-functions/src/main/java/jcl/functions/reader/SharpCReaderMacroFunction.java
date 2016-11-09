/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Iterator;
import java.util.Optional;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.RealStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.reader.Reader;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

/**
 * Implements the '#c' Lisp reader macro.
 */
@Component
@DependsOn("readerBootstrap")
public class SharpCReaderMacroFunction extends ReaderMacroFunctionImpl {

	private final Reader reader;

	@Autowired
	public SharpCReaderMacroFunction(final Reader reader) {
		super("SHARP-C");
		this.reader = reader;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_C, this);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_C, this);
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_C) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_C);

		final LispStruct token = reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NILStruct.INSTANCE;
		}

		if (!(token instanceof ListStruct)) {
			throw new ReaderErrorException("Illegal complex number format: #C" + token);
		}

		final ListStruct listToken = (ListStruct) token;
		if (!listToken.isProper()) {
			throw new ReaderErrorException("Illegal complex number format: #C" + token);
		}

		final int maxNumberOfTokensForComplex = 2;
		if (listToken.length() != maxNumberOfTokensForComplex) {
			throw new ReaderErrorException("Illegal complex number format: #C" + token);
		}
		final Iterator<LispStruct> iterator = listToken.iterator();

		final LispStruct realToken = iterator.next();
		if (!(realToken instanceof RealStruct)) {
			throw new ReaderErrorException("Only real numbers are valid tokens for #c. Got: " + realToken);
		}

		final LispStruct imaginaryToken = iterator.next();
		if (!(imaginaryToken instanceof RealStruct)) {
			throw new ReaderErrorException("Only real numbers are valid tokens for #c. Got: " + imaginaryToken);
		}

		return LispStructFactory.toComplex((RealStruct) realToken, (RealStruct) imaginaryToken);
	}
}