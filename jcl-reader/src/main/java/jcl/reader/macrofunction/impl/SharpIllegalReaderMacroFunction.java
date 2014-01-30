package jcl.reader.macrofunction.impl;

import jcl.reader.macrofunction.MacroFunctionReader;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;

/**
 * Implements the illegal '#??" Lisp reader macros.
 */
public class SharpIllegalReaderMacroFunction implements ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) throws ReaderErrorException {
		throw new ReaderErrorException("Illegal sharp character " + codePoint);
	}
}
