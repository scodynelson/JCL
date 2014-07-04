package jcl.readtables.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.readtables.reader.function.ExtendedTokenReader;
import jcl.readtables.reader.impl.Reader;
import jcl.readtables.reader.syntax.ReadExtendedToken;
import jcl.symbols.SymbolStruct;
import jcl.syntax.CharacterConstants;
import jcl.readtables.reader.ReadSuppressVariable;

/**
 * Implements the '#:' Lisp reader macro.
 */
public class SharpColonReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.COLON;

		final ExtendedTokenReader macroFunctionReader = new ExtendedTokenReader(reader);
		final ReadExtendedToken readExtendedToken = macroFunctionReader.readExtendedToken(false);
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
