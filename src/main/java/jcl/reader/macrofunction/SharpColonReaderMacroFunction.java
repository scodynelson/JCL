package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.state.MacroFunctionReader;
import jcl.syntax.reader.ReadExtendedToken;
import jcl.syntax.CharacterConstants;
import jcl.symbols.SymbolStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.variables.ReadSuppressVariable;

/**
 * Implements the '#:' Lisp reader macro.
 */
public class SharpColonReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.COLON;

		final ReadExtendedToken readExtendedToken = reader.readExtendedToken();
		final String token = readExtendedToken.getToken();

		if (ReadSuppressVariable.INSTANCE.getValue()) {
			return null;
		}

		if (readExtendedToken.hasPackageDelimiter()) {
			throw new ReaderErrorException("Symbol following #: contains a package marker: " + token);
		}
		return new SymbolStruct(token);
	}
}
