package jcl.reader.macrofunction;

import jcl.reader.state.MacroFunctionReader;
import jcl.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;

/**
 * Implements the illegal '#??" Lisp reader macros.
 */
public class SharpIllegalReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		throw new ReaderErrorException("Illegal sharp character " + codePoint);
	}
}
