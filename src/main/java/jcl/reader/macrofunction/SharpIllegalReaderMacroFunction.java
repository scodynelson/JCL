package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.Reader;
import jcl.structs.conditions.exceptions.ReaderErrorException;

import java.math.BigInteger;

/**
 * Implements the illegal '#??" Lisp reader macros.
 */
public final class SharpIllegalReaderMacroFunction extends ReaderMacroFunction {

	public static final SharpIllegalReaderMacroFunction INSTANCE = new SharpIllegalReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpIllegalReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		throw new ReaderErrorException("Illegal sharp character " + codePoint);
	}
}
