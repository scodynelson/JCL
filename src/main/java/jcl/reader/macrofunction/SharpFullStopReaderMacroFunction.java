package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.state.MacroFunctionReader;
import jcl.syntax.CharacterConstants;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.variables.ReadEvalVariable;
import jcl.variables.ReadSuppressVariable;

/**
 * Implements the '#.' Lisp reader macro.
 */
public class SharpFullStopReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.FULL_STOP;

		final LispStruct lispToken = reader.read();
		if (ReadSuppressVariable.INSTANCE.getValue()) {
			return null;
		}

		if (!ReadEvalVariable.INSTANCE.getValue()) {
			throw new ReaderErrorException("Attempt to read #. while *READ-EVAL* is bound to NIL.");
		}

		// TODO: need to evaluate and return the evaluated result
		// Evaluate the lisp token

		return lispToken;
	}
}
