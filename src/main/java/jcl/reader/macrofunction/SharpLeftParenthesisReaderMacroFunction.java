/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.CharacterConstants;
import jcl.reader.Reader;
import jcl.structs.arrays.VectorStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.conditions.exceptions.SimpleErrorException;
import jcl.structs.conditions.exceptions.TypeErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.variables.Variable;
import org.apache.commons.collections4.CollectionUtils;

import java.math.BigInteger;
import java.util.List;

/**
 * Implements the '#(...)' Lisp reader macro.
 */
public final class SharpLeftParenthesisReaderMacroFunction extends ListReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpLeftParenthesisReaderMacroFunction INSTANCE = new SharpLeftParenthesisReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpLeftParenthesisReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.LEFT_PARENTHESIS;

		final ListStruct listToken = readList(reader);

		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		if (listToken == null) {
			throw new ReaderErrorException("Ill-formed vector: #");
		}

		if (listToken.isDotted()) {
			throw new ReaderErrorException("Ill-formed vector: #" + listToken);
		}

		final List<LispStruct> lispTokens = listToken.getAsJavaList();

		if (numArg == null) {
			try {
				return new VectorStruct<>(lispTokens);
			} catch (final TypeErrorException | SimpleErrorException e) {
				throw new ReaderErrorException("Error occurred creating vector.", e);
			}
		}

		final int numberOfTokens = lispTokens.size();
		final int numArgInt = numArg.intValueExact();
		if (numberOfTokens > numArgInt) {
			throw new ReaderErrorException("Vector is longer than specified length: #" + numArg + listToken);
		}

		LispStruct lastToken = null;
		if (CollectionUtils.isNotEmpty(lispTokens)) {
			lastToken = lispTokens.get(numberOfTokens - 1);
		}

		final int fillAmount = numArgInt - numberOfTokens;
		for (int i = 0; i < fillAmount; i++) {
			lispTokens.add(lastToken);
		}

		try {
			return new VectorStruct<>(lispTokens);
		} catch (final TypeErrorException | SimpleErrorException e) {
			throw new ReaderErrorException("Error occurred creating vector.", e);
		}
	}
}
