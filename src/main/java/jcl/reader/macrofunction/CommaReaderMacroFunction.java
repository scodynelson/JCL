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
import jcl.lists.ConsStruct;
import jcl.lists.NullStruct;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.streams.ReadPeekResult;
import jcl.system.CommonLispSymbols;
import org.springframework.stereotype.Component;

/**
 * Implements the ',' Lisp reader macro.
 */
@Component
public class CommaReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -8890411312426952661L;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setMacroCharacter(CharacterConstants.COMMA, this, false);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CharacterConstants.GRAVE_ACCENT;

		final int currentBackquoteLevel = reader.getBackquoteLevel();
		if (currentBackquoteLevel <= 0) {
			if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
				return NullStruct.INSTANCE;
			}

			throw new ReaderErrorException("Comma not inside a backquote.");
		}

		final ReadPeekResult readResult = reader.readChar(true, NullStruct.INSTANCE, false);
		final int nextCodePoint = readResult.getResult();

		reader.decrementBackquoteLevel();
		try {
			final ConsStruct commaCons;

			if (nextCodePoint == CharacterConstants.AT_SIGN) {
				final LispStruct token = reader.read(true, NullStruct.INSTANCE, true);
				commaCons = new ConsStruct(CommonLispSymbols.BQ_AT_FLAG, token);
			} else if (nextCodePoint == CharacterConstants.FULL_STOP) {
				final LispStruct token = reader.read(true, NullStruct.INSTANCE, true);
				commaCons = new ConsStruct(CommonLispSymbols.BQ_DOT_FLAG, token);
			} else {
				reader.unreadChar(nextCodePoint);
				final LispStruct token = reader.read(true, NullStruct.INSTANCE, true);
				commaCons = new ConsStruct(CommonLispSymbols.BQ_COMMA_FLAG, token);
			}
			return commaCons;
		} finally {
			reader.incrementBackquoteLevel();
		}
	}
}
