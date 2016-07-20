/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.lang.LispStruct;
import jcl.lang.SpecialOperatorStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.function.ReaderMacroFunction;
import jcl.lang.list.ListStruct;
import jcl.lang.list.NILStruct;
import jcl.lang.readtable.Reader;
import jcl.lang.readtable.ReaderVariables;
import jcl.util.CodePointConstants;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.stereotype.Component;

/**
 * Implements the '#'' Lisp reader macro.
 */
@Component
public class SharpApostropheReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.APOSTROPHE, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.APOSTROPHE;

		final LispStruct token = reader.read(true, NILStruct.INSTANCE, true);
		if (token == null) {
			throw new ReaderErrorException("Missing expression.");
		}

		return ListStruct.buildProperList(SpecialOperatorStruct.FUNCTION, token);
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).toString();
	}
}
