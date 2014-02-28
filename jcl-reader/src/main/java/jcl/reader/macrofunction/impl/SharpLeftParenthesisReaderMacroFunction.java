package jcl.reader.macrofunction.impl;

import jcl.reader.macrofunction.MacroFunctionReader;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.LispStruct;
import jcl.structs.arrays.VectorStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.conditions.exceptions.SimpleErrorException;
import jcl.structs.conditions.exceptions.TypeErrorException;
import jcl.structs.conses.ListStruct;
import jcl.types.Variable;
import org.apache.commons.collections4.CollectionUtils;

import java.util.List;

/**
 * Implements the '#(...)' Lisp reader macro.
 */
public class SharpLeftParenthesisReaderMacroFunction implements ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.LEFT_PARENTHESIS;

		final ListStruct listToken = reader.readList();

		if (Variable.ReadSuppress) {
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
		if (numberOfTokens > numArg) {
			throw new ReaderErrorException("Vector is longer than specified length: #" + numArg + listToken);
		} else {
			final LispStruct lastToken;
			if (CollectionUtils.isEmpty(lispTokens)) {
				lastToken = null;
			} else {
				lastToken = lispTokens.get(numberOfTokens - 1);
			}

			final int fillAmount = numArg - numberOfTokens;
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
}
