package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.Reader;
import jcl.reader.syntax.CharacterConstants;
import jcl.reader.syntax.ReadExtendedToken;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.symbols.SymbolStruct;
import jcl.structs.symbols.variables.Variable;

import java.math.BigInteger;

/**
 * Implements the '#:' Lisp reader macro.
 */
public class SharpColonReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.COLON;

		final ExtendedTokenReader macroFunctionReader = new ExtendedTokenReader(reader, false);
		final ReadExtendedToken readExtendedToken = macroFunctionReader.process();
		final String token = readExtendedToken.getToken();

		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		if (readExtendedToken.hasPackageDelimiter()) {
			throw new ReaderErrorException("Symbol following #: contains a package marker: " + token);
		}
		return new SymbolStruct<>(token);
	}
}
