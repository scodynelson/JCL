package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.CharacterConstants;
import jcl.reader.Reader;
import jcl.structs.arrays.StringStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.pathnames.PathnameStruct;
import jcl.structs.symbols.variables.Variable;

import java.math.BigInteger;
import java.net.URISyntaxException;

/**
 * Implements the '#p' Lisp reader macro.
 */
public final class SharpPReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpPReaderMacroFunction INSTANCE = new SharpPReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpPReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_P) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_P);

		final LispStruct lispStruct = reader.read();
		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		if (lispStruct instanceof StringStruct) {
			final String javaString = ((StringStruct) lispStruct).getAsJavaString();
			try {
				return PathnameStruct.buildPathname(javaString);
			} catch (final URISyntaxException use) {
				throw new ReaderErrorException("Improper namestring provided to #P: " + lispStruct, use);
			}
		} else {
			throw new ReaderErrorException("Improper namestring provided to #P: " + lispStruct);
		}
	}
}
