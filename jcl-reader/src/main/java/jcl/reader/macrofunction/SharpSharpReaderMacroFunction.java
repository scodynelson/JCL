/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.readtable.Reader;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.springframework.stereotype.Component;

/**
 * Implements the '##' Lisp reader macro.
 */
@Component
public class SharpSharpReaderMacroFunction extends ReaderMacroFunctionImpl {

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.NUMBER_SIGN, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.NUMBER_SIGN;

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NILStruct.INSTANCE;
		}

		if (!numberArgument.isPresent()) {
			throw new ReaderErrorException("Missing label for ##.");
		}
		final BigInteger numberArgumentValue = numberArgument.get();

		final LispStruct labelToken = reader.getSharpEqualFinalTable().get(numberArgumentValue);
		if (labelToken != null) {
			return labelToken;
		}

		final SymbolStruct possibleLabelTag = reader.getSharpEqualTempTable().get(numberArgumentValue);
		if (possibleLabelTag != null) {
			return possibleLabelTag;
		}

		throw new ReaderErrorException("Reference to undefined label #" + numberArgumentValue + '#');
	}
}
