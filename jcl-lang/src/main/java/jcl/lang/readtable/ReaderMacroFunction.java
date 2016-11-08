package jcl.lang.readtable;

import jcl.lang.FunctionStruct;

/**
 * Abstract implementation definition for all Reader defined macro functions that read character macros based off of a
 * provided {@link Integer} code point.
 */
public interface ReaderMacroFunction extends FunctionStruct {

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
