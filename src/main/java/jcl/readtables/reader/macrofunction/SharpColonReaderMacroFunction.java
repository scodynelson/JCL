package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.readtables.reader.impl.macrofunctions.ExtendedTokenMacroFunctionReader;
import jcl.readtables.reader.LispReader;
import jcl.symbols.SymbolStruct;
import jcl.syntax.CharacterConstants;
import jcl.readtables.reader.syntax.ReadExtendedToken;
import jcl.variables.ReadSuppressVariable;

/**
 * Implements the '#:' Lisp reader macro.
 */
public class SharpColonReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final LispReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.COLON;

		final ExtendedTokenMacroFunctionReader macroFunctionReader = new ExtendedTokenMacroFunctionReader(reader);
		final ReadExtendedToken readExtendedToken = macroFunctionReader.readExtendedToken();
		final String token = readExtendedToken.getToken();

		if (ReadSuppressVariable.INSTANCE.getValue()) {
			return null;
		}

		if (readExtendedToken.hasPackageDelimiter()) {
			throw new ReaderErrorException("Symbol following #: contains a package marker: " + token);
		}
		return new SymbolStruct<>(token);
	}
}
