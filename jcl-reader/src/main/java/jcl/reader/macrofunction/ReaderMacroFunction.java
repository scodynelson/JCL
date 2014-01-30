package jcl.reader.macrofunction;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;

/**
 * Defines a ReaderMacroFunction type.
 */
public interface ReaderMacroFunction {

	/**
	 * Interpret the character stream (up to EOF or new line) as a token of the T type supplied.
	 *
	 * @param codePoint the character that determines the macro function
	 * @param reader    the reader used to read characters
	 * @param numArg    the optional number argument
	 * @return a LispStruct
	 * @throws ReaderErrorException if an error is encountered
	 */
	LispStruct readMacro(int codePoint, MacroFunctionReader reader, Integer numArg) throws ReaderErrorException;
}
