package jcl.reader.macrofunction.impl;

import jcl.reader.macrofunction.MacroFunctionReader;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.pathnames.PathnameStruct;
import jcl.structs.strings.StringStruct;
import jcl.types.Variable;

/**
 * Implements the '#p' Lisp reader macro.
 */
public class SharpPReaderMacroFunction implements ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) throws ReaderErrorException {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_P) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_P);

		final LispStruct lispStruct = reader.read();
		if (Variable.ReadSuppress) {
			return null;
		} else {
			if (lispStruct instanceof StringStruct) {
				final String javaString = ((StringStruct) lispStruct).getAsJavaString();
				return PathnameStruct.getStruct(javaString);
			} else {
				throw new ReaderErrorException("Improper namestring provided to #P: " + lispStruct);
			}
		}
	}
}
