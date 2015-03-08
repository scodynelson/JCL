/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.streams.ReadPeekResult;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;

/**
 * Implements the ',' Lisp reader macro.
 */
@Component
public class CommaReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -8890411312426952661L;

	static final SymbolStruct<?> BQ_COMMA_FLAG = new SymbolStruct<>(",", GlobalPackageStruct.BACKQUOTE);

	static final SymbolStruct<?> BQ_AT_FLAG = new SymbolStruct<>(",@", GlobalPackageStruct.BACKQUOTE);

	static final SymbolStruct<?> BQ_DOT_FLAG = new SymbolStruct<>(",.", GlobalPackageStruct.BACKQUOTE);

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setMacroCharacter(CharacterConstants.COMMA, this, false);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
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
			final ConsStruct consStruct;

			if (nextCodePoint == CharacterConstants.AT_SIGN) {
				final LispStruct code = reader.read(true, NullStruct.INSTANCE, true);
				consStruct = new ConsStruct(BQ_AT_FLAG, code);
			} else if (nextCodePoint == CharacterConstants.FULL_STOP) {
				final LispStruct code = reader.read(true, NullStruct.INSTANCE, true);
				consStruct = new ConsStruct(BQ_DOT_FLAG, code);
			} else {
				reader.unreadChar(nextCodePoint);
				final LispStruct code = reader.read(true, NullStruct.INSTANCE, true);
				consStruct = new ConsStruct(BQ_COMMA_FLAG, code);
			}
			return consStruct;
		} finally {
			reader.incrementBackquoteLevel();
		}
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
