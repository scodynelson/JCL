package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.readtables.reader.Reader;
import jcl.syntax.CharacterConstants;
import jcl.readtables.reader.ReadSuppressVariable;

import java.util.UUID;

/**
 * Implements the '##' Lisp reader macro.
 */
public class SharpSharpReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.NUMBER_SIGN;

		if (ReadSuppressVariable.INSTANCE.getValue()) {
			return null;
		}

		if (numArg == null) {
			throw new ReaderErrorException("Missing label for ##.");
		}

		final LispStruct labelObject = SharpTagReaderConstants.SHARP_EQUAL_FINAL_TABLE.get(numArg);
		if (labelObject != null) {
			return labelObject;
		}

		final UUID possibleLabelTag = SharpTagReaderConstants.SHARP_EQUAL_TEMP_TABLE.get(numArg);
		final LispStruct possibleLabelObject = SharpTagReaderConstants.SHARP_EQUAL_REPL_TABLE.get(possibleLabelTag);
		if (possibleLabelObject != null) {
			return possibleLabelObject;
		}

		throw new ReaderErrorException("Reference to undefined label #" + numArg + '#');
	}
}
