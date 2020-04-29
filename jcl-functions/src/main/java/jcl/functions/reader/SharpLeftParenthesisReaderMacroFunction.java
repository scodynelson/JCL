/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import jcl.lang.ConsStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.VectorStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.ReaderVariables;
import jcl.reader.ReaderContext;
import jcl.reader.ReaderContextHolder;
import jcl.util.CodePointConstants;
import org.apache.commons.collections4.CollectionUtils;

/**
 * Implements the '#(...)' Lisp reader macro.
 */
public final class SharpLeftParenthesisReaderMacroFunction extends ReaderMacroFunctionImpl {

	public SharpLeftParenthesisReaderMacroFunction() {
		super("SHARP-LEFT-PARENTHESIS");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.LEFT_PARENTHESIS;

		final ListStruct listToken = ListReaderMacroFunction.readList(inputStreamStruct);

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().toJavaPBoolean()) {
			return NILStruct.INSTANCE;
		}

		if (listToken == null) {
			throw new ReaderErrorException("Ill-formed vector: #");
		}

		if (!listToken.isProper()) {
			throw new ReaderErrorException("Ill-formed vector: #" + listToken);
		}

		final ReaderContext context = ReaderContextHolder.getContext();
		final int backquoteLevel = context.getBackquoteLevel();
		if (backquoteLevel == 0) {
			if (!numberArgument.isPresent()) {
				final List<LispStruct> tokensAsJavaList = listToken.stream().collect(Collectors.toList());
				final IntegerStruct size = IntegerStruct.toLispInteger(tokensAsJavaList.size());
				return VectorStruct.toLispVector(size, CommonLispSymbols.T, tokensAsJavaList);
			}

			final BigInteger numberArgumentValue = numberArgument.get();
			return handleNumberArgument(listToken, numberArgumentValue);
		}

		return ConsStruct.toLispCons(BackquoteReaderMacroFunction.BQ_VECTOR_FLAG, listToken);
	}

	/**
	 * Handles the processing of the number argument when parsing the provided list of {@link LispStruct}s into a
	 * {@link VectorStruct}.
	 *
	 * @param listToken
	 * 		the vector contents
	 * @param numberArgument
	 * 		the number argument passed to be used as the vector length
	 *
	 * @return the properly created {@link VectorStruct} taking care of the proper vector length
	 */
	private static VectorStruct handleNumberArgument(final ListStruct listToken, final BigInteger numberArgument) {
		final List<LispStruct> tokensAsJavaList = listToken.stream().collect(Collectors.toList());

		final int numberOfTokens = tokensAsJavaList.size();
		final int numberArgumentIntValue = numberArgument.intValueExact();
		if (numberOfTokens > numberArgumentIntValue) {
			throw new ReaderErrorException("Vector is longer than specified length: #" + numberArgument + listToken);
		}

		LispStruct lastToken = null;
		if (CollectionUtils.isNotEmpty(tokensAsJavaList)) {
			lastToken = tokensAsJavaList.get(numberOfTokens - 1);
		}

		final int fillAmount = numberArgumentIntValue - numberOfTokens;
		for (int i = 0; i < fillAmount; i++) {
			tokensAsJavaList.add(lastToken);
		}

		final IntegerStruct size = IntegerStruct.toLispInteger(tokensAsJavaList.size());
		return VectorStruct.toLispVector(size, CommonLispSymbols.T, tokensAsJavaList);
	}
}
