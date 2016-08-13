/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.ListStruct;
import jcl.lang.readtable.Reader;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the '(...)' Lisp reader macro.
 */
@Component
public class LeftParenthesisReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * {@link Autowired} {@link ListReaderMacroFunction} used for reading {@link ListStruct}s.
	 */
	private final ListReaderMacroFunction listReaderMacroFunction;

	@Autowired
	public LeftParenthesisReaderMacroFunction(final ListReaderMacroFunction listReaderMacroFunction) {
		this.listReaderMacroFunction = listReaderMacroFunction;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setMacroCharacter(CodePointConstants.LEFT_PARENTHESIS, this, false);
	}

	@Override
	public ListStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.LEFT_PARENTHESIS;

		return listReaderMacroFunction.readList(reader);
	}
}
