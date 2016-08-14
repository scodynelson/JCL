package jcl.lang.readtable;

/**
 * This holds mappings for code points to {@link ReaderMacroFunction}s and delegates to the proper one when used.
 */
public interface DispatchingReaderMacroFunction extends ReaderMacroFunction {

	ReaderMacroFunction getMacroFunction(final int codePoint);

	/**
	 * Sets the {@link ReaderMacroFunction} with the provided {@code codePoint} to the provided {@code
	 * readerMacroFunction}.
	 *
	 * @param codePoint
	 * 		the code point associated with the {@link ReaderMacroFunction} to set
	 * @param readerMacroFunction
	 * 		the new {@link ReaderMacroFunction} to be associated
	 */
	void setMacroCharacter(final int codePoint, final ReaderMacroFunction readerMacroFunction);

	@Override
	default boolean isDispatch() {
		return true;
	}
}
