package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.reader.impl.Reader;
import jcl.structs.conditions.exceptions.ReaderErrorException;

import java.math.BigInteger;

/**
 * Implements the illegal '#??" Lisp reader macros.
 */
public class SharpIllegalReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		throw new ReaderErrorException("Illegal sharp character " + codePoint);
	}
}
