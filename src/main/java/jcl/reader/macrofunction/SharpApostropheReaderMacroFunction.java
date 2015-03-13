/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.system.CommonLispSymbols;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.stereotype.Component;

/**
 * Implements the '#'' Lisp reader macro.
 */
@Component
public class SharpApostropheReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -480798662778934982L;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.APOSTROPHE, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numberArgument) {
		assert codePoint == CharacterConstants.APOSTROPHE;

		final LispStruct token = reader.read(true, NullStruct.INSTANCE, true);
		if (token == null) {
			throw new ReaderErrorException("Missing expression.");
		}

		return ListStruct.buildProperList(CommonLispSymbols.FUNCTION, token);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
