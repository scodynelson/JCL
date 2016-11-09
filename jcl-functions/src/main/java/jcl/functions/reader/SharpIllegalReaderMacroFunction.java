/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

/**
 * Implements the illegal '#??" Lisp reader macros.
 */
@Component
@DependsOn("readerBootstrap")
public class SharpIllegalReaderMacroFunction extends ReaderMacroFunctionImpl {

	protected SharpIllegalReaderMacroFunction() {
		super("SHARP-ILLEGAL");
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.RIGHT_PARENTHESIS, this);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LESS_THAN_SIGN, this);

		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.TAB, this);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.NEWLINE, this);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LINE_FEED, this);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.SPACE, this);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.PAGE, this);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.RETURN, this);
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		throw new ReaderErrorException("Illegal sharp character " + codePoint);
	}
}
