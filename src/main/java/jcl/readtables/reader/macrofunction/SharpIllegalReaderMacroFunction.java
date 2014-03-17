package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.readtables.reader.impl.macrofunctions.MacroFunctionReader;
import jcl.conditions.exceptions.ReaderErrorException;

/**
 * Implements the illegal '#??" Lisp reader macros.
 */
public class SharpIllegalReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		throw new ReaderErrorException("Illegal sharp character " + codePoint);
	}
}
