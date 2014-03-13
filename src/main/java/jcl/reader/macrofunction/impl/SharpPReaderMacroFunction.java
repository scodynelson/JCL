package jcl.reader.macrofunction.impl;

import jcl.reader.macrofunction.MacroFunctionReader;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.PathnameStruct;
import jcl.structs.StringStruct;
import jcl.structs.symbols.Variable;

import java.net.URISyntaxException;

/**
 * Implements the '#p' Lisp reader macro.
 */
public class SharpPReaderMacroFunction implements ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_P) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_P);

		final LispStruct lispStruct = reader.read();
		if (Variable.ReadSuppress) {
			return null;
		} else {
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
}
