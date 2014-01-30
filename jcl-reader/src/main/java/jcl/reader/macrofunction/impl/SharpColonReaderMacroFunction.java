package jcl.reader.macrofunction.impl;

import jcl.reader.macrofunction.MacroFunctionReader;
import jcl.reader.macrofunction.ReadExtendedToken;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.symbols.SymbolStruct;
import jcl.types.Variable;

/**
 * Implements the '#:' Lisp reader macro.
 */
public class SharpColonReaderMacroFunction implements ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) throws ReaderErrorException {
		assert codePoint == CharacterConstants.COLON;

		final ReadExtendedToken readExtendedToken = reader.readExtendedToken();
		final String token = readExtendedToken.getToken();

		if (Variable.ReadSuppress) {
			return null;
		} else if (readExtendedToken.hasPackageDelimiter()) {
			throw new ReaderErrorException("Symbol following #: contains a package marker: " + token);
		} else {
			return SymbolStruct.getStruct(token);
		}
	}
}
