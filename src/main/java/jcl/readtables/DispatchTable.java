package jcl.readtables;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.readtables.reader.Reader;
import jcl.readtables.reader.function.macrofunction.ReaderMacroFunction;
import jcl.syntax.reader.ReadResult;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The {@code DispatchTable} class holds mappings for code points to macro functions and delegates to the proper function
 * when used.
 */
public class DispatchTable extends ReaderMacroFunction {

	private final Map<Integer, ReaderMacroFunction> macroFunctionMap = new ConcurrentHashMap<>();

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {

		final ReadResult readResult = reader.readChar(false, null, false);
		if (readResult.wasEOF()) {
			throw new ReaderErrorException("End of file reached when trying to determine read macro function.");
		}

		final int readChar = readResult.getResult();
		final ReaderMacroFunction macroFunction = getMacroFunction(readChar);
		if (macroFunction == null) {
			throw new ReaderErrorException("No reader macro function exists for: " + codePoint + readChar + '.');
		}

		return macroFunction.readMacro(readChar, reader, numArg);
	}

	/**
	 * Gets the macro function associated with the provided {@code codePoint}, or null if no such function exists.
	 *
	 * @param codePoint the code point associated with the macro function to retrieve
	 * @return the macro function associated with the provided {@code codePoint}, or null if no such function exists
	 */
	public ReaderMacroFunction getMacroFunction(final int codePoint) {
		return macroFunctionMap.get(codePoint);
	}

	/**
	 * Sets the macro function with the provided {@code codePoint} to the provided {@code macroFunction}.
	 *
	 * @param codePoint     the code point associated with the macro function to set
	 * @param macroFunction the new macro function to be associated
	 */
	public void setMacroCharacter(final int codePoint, final ReaderMacroFunction macroFunction) {
		macroFunctionMap.put(codePoint, macroFunction);
	}

	@Override
	public String toString() {
		return "DispatchTable{"
				+ "macroFunctionMap=" + macroFunctionMap
				+ '}';
	}
}
