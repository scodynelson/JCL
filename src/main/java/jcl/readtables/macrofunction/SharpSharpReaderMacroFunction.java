package jcl.readtables.macrofunction;

import jcl.LispStruct;
import jcl.readtables.state.MacroFunctionReader;
import jcl.syntax.CharacterConstants;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.variables.ReadSuppressVariable;

/**
 * Implements the '##' Lisp reader macro.
 */
public class SharpSharpReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.NUMBER_SIGN;

		if (ReadSuppressVariable.INSTANCE.getValue()) {
			return null;
		}

		if (numArg == null) {
			throw new ReaderErrorException("Missing label for ##.");
		}

		final LispStruct labelObject = reader.SHARP_EQUAL_FINAL_TABLE.get(numArg);
		if (labelObject != null) {
			return labelObject;
		}

		final Object possibleLabelObject = reader.SHARP_EQUAL_TEMP_TABLE.get(numArg);
		if (possibleLabelObject instanceof LispStruct) {
			return (LispStruct) possibleLabelObject;
		}

		throw new ReaderErrorException("Reference to undefined label #" + numArg + '#');
	}
}
