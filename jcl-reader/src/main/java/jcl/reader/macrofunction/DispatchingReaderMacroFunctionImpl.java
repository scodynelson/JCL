package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.LispStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.readtable.DispatchingReaderMacroFunction;
import jcl.reader.Reader;
import jcl.lang.readtable.ReaderInputStreamStruct;
import jcl.lang.readtable.ReaderMacroFunction;
import jcl.lang.stream.ReadPeekResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

/**
 * This holds mappings for code points to {@link ReaderMacroFunction}s and delegates to the proper one when used.
 */
@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public final class DispatchingReaderMacroFunctionImpl extends ReaderMacroFunctionImpl implements DispatchingReaderMacroFunction {

	/**
	 * Internal map storing character code points to {@link ReaderMacroFunction}s to dispatch on when reading.
	 */
	private final Map<Integer, ReaderMacroFunction> readerMacroFunctionMap = new ConcurrentHashMap<>();

	private final Reader reader;

	@Autowired
	public DispatchingReaderMacroFunctionImpl(final Reader reader) {
		this.reader = reader;
	}

	@Override
	public LispStruct readMacro(final ReaderInputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {

		final ReadPeekResult readResult = reader.readChar(inputStreamStruct, false, null, false);
		if (readResult.isEof()) {
			throw new ReaderErrorException("End of file reached when trying to determine read macro function.");
		}

		final int nextCodePoint = readResult.getResult();
		final ReaderMacroFunction macroFunction = getMacroFunction(nextCodePoint);
		if (macroFunction == null) {
			throw new ReaderErrorException("No reader macro function exists for: " + codePoint + nextCodePoint + '.');
		}

		return macroFunction.readMacro(inputStreamStruct, nextCodePoint, numberArgument);
	}

	/**
	 * Gets the {@link ReaderMacroFunction} associated with the provided {@code codePoint}, or null if no such
	 * function exists.
	 *
	 * @param codePoint
	 * 		the code point associated with the {@link ReaderMacroFunction} to retrieve
	 *
	 * @return the {@link ReaderMacroFunction} associated with the provided {@code codePoint}, or null if no such
	 * function exists
	 */
	@Override
	public ReaderMacroFunction getMacroFunction(final int codePoint) {
		return readerMacroFunctionMap.get(codePoint);
	}

	/**
	 * Sets the {@link ReaderMacroFunction} with the provided {@code codePoint} to the provided {@code
	 * readerMacroFunction}.
	 *
	 * @param codePoint
	 * 		the code point associated with the {@link ReaderMacroFunction} to set
	 * @param readerMacroFunction
	 * 		the new {@link ReaderMacroFunction} to be associated
	 */
	@Override
	public void setMacroCharacter(final int codePoint, final ReaderMacroFunction readerMacroFunction) {
		readerMacroFunctionMap.put(codePoint, readerMacroFunction);
	}
}
