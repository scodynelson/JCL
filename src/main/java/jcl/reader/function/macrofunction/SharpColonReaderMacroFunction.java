package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.reader.function.ExtendedTokenReader;
import jcl.reader.impl.Reader;
import jcl.reader.syntax.ReadExtendedToken;
import jcl.structs.symbols.SymbolStruct;
import jcl.syntax.CharacterConstants;
import jcl.variables.ReadSuppressVariable;

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
