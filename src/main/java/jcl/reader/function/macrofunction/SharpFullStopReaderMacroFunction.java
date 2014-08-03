package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.reader.impl.Reader;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.symbols.Variable;
import jcl.syntax.CharacterConstants;

import java.math.BigInteger;

/**
 * Implements the '#.' Lisp reader macro.
 */
public class SharpFullStopReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.FULL_STOP;

		final LispStruct lispToken = reader.read();
		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		if (!Variable.READ_EVAL.getValue().booleanValue()) {
			throw new ReaderErrorException("Attempt to read #. while *READ-EVAL* is bound to NIL.");
		}

		// TODO: need to evaluate and return the evaluated result
		// Evaluate the lisp token

		return lispToken;
	}
}