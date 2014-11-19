package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.reader.Reader;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.functions.FunctionStruct;

import java.math.BigInteger;

/**
 * Defines a ReaderMacroFunction type.
 */
public abstract class ReaderMacroFunction extends FunctionStruct {

	/**
	 * Interpret the character stream (up to EOF or new line) as a token of the T type supplied.
	 *
	 * @param codePoint
	 * 		the character that determines the macro function
	 * @param reader
	 * 		the reader used to read characters
	 * @param numArg
	 * 		the optional number argument
	 *
	 * @return a LispStruct
	 *
	 * @throws ReaderErrorException
	 * 		if an error is encountered
	 */
	public abstract LispStruct readMacro(int codePoint, Reader reader, BigInteger numArg);

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return null; // TODO: do this
	}
}
