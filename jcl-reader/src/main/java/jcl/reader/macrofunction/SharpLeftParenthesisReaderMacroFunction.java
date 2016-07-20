/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.lang.character.CharacterConstants;
import jcl.lang.list.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.list.ListStruct;
import jcl.lang.list.NILStruct;
import jcl.lang.array.VectorStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.function.ReaderMacroFunction;
import jcl.lang.readtable.Reader;
import jcl.lang.readtable.ReaderVariables;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the '#(...)' Lisp reader macro.
 */
@Component
public class SharpLeftParenthesisReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * {@link Autowired} {@link ListReaderMacroFunction} used for reading {@link ListStruct}s.
	 */
	@Autowired
	private ListReaderMacroFunction listReaderMacroFunction;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LEFT_PARENTHESIS, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CharacterConstants.LEFT_PARENTHESIS;

		final ListStruct listToken = listReaderMacroFunction.readList(reader);

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NILStruct.INSTANCE;
		}

		if (listToken == null) {
			throw new ReaderErrorException("Ill-formed vector: #");
		}

		if (!listToken.isProper()) {
			throw new ReaderErrorException("Ill-formed vector: #" + listToken);
		}

		final int backquoteLevel = reader.getBackquoteLevel();
		if (backquoteLevel == 0) {
			if (!numberArgument.isPresent()) {
				final List<LispStruct> tokensAsJavaList = listToken.stream().collect(Collectors.toList());
				return new VectorStruct<>(tokensAsJavaList);
			}

			final BigInteger numberArgumentValue = numberArgument.get();
			return handleNumberArgument(listToken, numberArgumentValue);
		}

		return new ConsStruct(BackquoteReaderMacroFunction.BQ_VECTOR_FLAG, listToken);
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
	private static VectorStruct<?> handleNumberArgument(final ListStruct listToken, final BigInteger numberArgument) {
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

		return new VectorStruct<>(tokensAsJavaList);
	}
}
