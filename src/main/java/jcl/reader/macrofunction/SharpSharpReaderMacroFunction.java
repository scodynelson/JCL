/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

/**
 * Implements the '##' Lisp reader macro.
 */
@Component
public class SharpSharpReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.NUMBER_SIGN, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CharacterConstants.NUMBER_SIGN;

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
