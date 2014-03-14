package jcl.reader.macrofunction;

import jcl.structs.FunctionStruct;
import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;

/**
 * Defines a ReaderMacroFunction type.
 */
public abstract class ReaderMacroFunction extends FunctionStruct {

	/**
	 * Interpret the character stream (up to EOF or new line) as a token of the T type supplied.
	 *
	 * @param codePoint the character that determines the macro function
	 * @param reader    the reader used to read characters
	 * @param numArg    the optional number argument
	 * @return a LispStruct
	 * @throws ReaderErrorException if an error is encountered
	 */
	public abstract LispStruct readMacro(int codePoint, MacroFunctionReader reader, Integer numArg);

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return null; // TODO: do this
	}
}
