package jcl.readtables.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.arrays.VectorStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.lists.ListStruct;
import jcl.readtables.reader.function.ListReader;
import jcl.readtables.reader.impl.Reader;
import jcl.syntax.CharacterConstants;
import jcl.readtables.reader.ReadSuppressVariable;
import org.apache.commons.collections4.CollectionUtils;

import java.util.List;

/**
 * Implements the '#(...)' Lisp reader macro.
 */
public class SharpLeftParenthesisReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.LEFT_PARENTHESIS;

		final ListReader macroFunctionReader = new ListReader(reader);
		final ListStruct listToken = macroFunctionReader.readList();

		if (ReadSuppressVariable.INSTANCE.getValue()) {
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
		}

		LispStruct lastToken = null;
		if (CollectionUtils.isNotEmpty(lispTokens)) {
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
