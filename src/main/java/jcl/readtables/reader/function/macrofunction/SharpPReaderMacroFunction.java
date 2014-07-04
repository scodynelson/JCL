package jcl.readtables.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.pathnames.PathnameStruct;
import jcl.readtables.reader.impl.Reader;
import jcl.syntax.CharacterConstants;
import jcl.readtables.reader.ReadSuppressVariable;

import java.net.URISyntaxException;

/**
 * Implements the '#p' Lisp reader macro.
 */
public class SharpPReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_P) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_P);

		final LispStruct lispStruct = reader.read();
		if (ReadSuppressVariable.INSTANCE.getValue()) {
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
