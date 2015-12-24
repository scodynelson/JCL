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
import org.springframework.stereotype.Component;

/**
 * Implements the ')' Lisp reader macro.
 */
@Component
public class RightParenthesisReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 8031891296893301768L;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getVariableValue().setMacroCharacter(CharacterConstants.RIGHT_PARENTHESIS, this, false);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CharacterConstants.RIGHT_PARENTHESIS;

		throw new ReaderErrorException("Unmatched close parenthesis.");
	}
}
