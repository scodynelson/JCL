package jcl.reader.macrofunction.impl;

import jcl.reader.macrofunction.MacroFunctionReader;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.symbols.Variable;

/**
 * Implements the '#.' Lisp reader macro.
 */
public class SharpFullStopReaderMacroFunction implements ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.FULL_STOP;

		final LispStruct lispToken = reader.read();

		if (Variable.ReadSuppress) {
			return null;
		}

		if (!Variable.ReadEval) {
			throw new ReaderErrorException("Attempt to read #. while *READ-EVAL* is bound to NIL.");
		}

		// TODO: need to evaluate and return the evaluated result
		// Evaluate the lisp token

		return lispToken;
	}
}
