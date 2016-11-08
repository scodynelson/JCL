package jcl.lang.readtable;

import jcl.lang.FunctionStruct;

/**
 * This holds mappings for code points to {@link FunctionStruct}s and delegates to the proper one when used.
 */
public interface DispatchingReaderMacroFunction extends FunctionStruct {

	FunctionStruct getMacroFunction(final int codePoint);

	/**
	 * Sets the {@link FunctionStruct} with the provided {@code codePoint} to the provided {@code
	 * readerMacroFunction}.
	 *
	 * @param codePoint
	 * 		the code point associated with the {@link FunctionStruct} to set
	 * @param readerMacroFunction
	 * 		the new {@link FunctionStruct} to be associated
	 */
	void setMacroCharacter(final int codePoint, final FunctionStruct readerMacroFunction);
}
