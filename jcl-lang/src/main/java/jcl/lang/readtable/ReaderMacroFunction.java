package jcl.lang.readtable;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;

/**
 * Abstract implementation definition for all Reader defined macro functions that read character macros based off of a
 * provided {@link Integer} code point.
 */
public interface ReaderMacroFunction extends FunctionStruct {

	/**
	 * Interpret the character stream from the provided {@link ReaderInputStreamStruct} (up to End-of-File or new line) based on the
	 * provided {@code codePoint}.
	 *
	 * @param inputStreamStruct
	 * 		the {@link ReaderInputStreamStruct} to read tokens from
	 * @param codePoint
	 * 		the character code point that determines the macro function
	 * @param numberArgument
	 * 		the optional number argument
	 *
	 * @return the parsed {@link LispStruct} token
	 */
	LispStruct readMacro(ReaderInputStreamStruct inputStreamStruct, int codePoint, Optional<BigInteger> numberArgument);

	/**
	 * Default method used to determine if the ReaderMacroFunction is a dispatching macro. The default value return is
	 * {@code #false}, however this is overridden in the internal dispatching table in a readtable.
	 *
	 * @return whether or not the ReaderMacroFunction is a dispatching macro
	 */
	default boolean isDispatch() {
		return false;
	}
}
