/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;

import jcl.compiler.functions.EvalFunction;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.readtable.Reader;
import jcl.lang.readtable.ReaderInputStreamStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

/**
 * Implements the '#.' Lisp reader macro.
 */
@Component
@DependsOn("readerBootstrap")
public class SharpFullStopReaderMacroFunction extends ReaderMacroFunctionImpl {

	private final Reader reader;

	/**
	 * {@link EvalFunction} singleton used to evaluate the expression passed to '#.'.
	 */
	private final EvalFunction evalFunction;

	@Autowired
	public SharpFullStopReaderMacroFunction(final Reader reader, final EvalFunction evalFunction) {
		this.reader = reader;
		this.evalFunction = evalFunction;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.FULL_STOP, this);
	}

	@Override
	public LispStruct readMacro(final ReaderInputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.FULL_STOP;

		final LispStruct token = reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NILStruct.INSTANCE;
		}

		if (!ReaderVariables.READ_EVAL.getVariableValue().booleanValue()) {
			throw new ReaderErrorException("Attempt to read #. while *READ-EVAL* is bound to NIL.");
		}

		return evalFunction.eval(token);
	}
}
