package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.FunctionStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.readtable.DispatchingReaderMacroFunction;
import jcl.reader.Reader;
import jcl.lang.stream.ReadPeekResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

/**
 * This holds mappings for code points to {@link FunctionStruct}s and delegates to the proper one when used.
 */
@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public final class DispatchingReaderMacroFunctionImpl extends ReaderMacroFunctionImpl implements DispatchingReaderMacroFunction {

	/**
	 * Internal map storing character code points to {@link FunctionStruct}s to dispatch on when reading.
	 */
	private final Map<Integer, FunctionStruct> readerMacroFunctionMap = new ConcurrentHashMap<>();

	private final Reader reader;

	@Autowired
	public DispatchingReaderMacroFunctionImpl(final Reader reader) {
		super("DISPATCHING");
		this.reader = reader;
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {

		final ReadPeekResult readResult = reader.readChar(inputStreamStruct, false, null, false);
		if (readResult.isEof()) {
			throw new ReaderErrorException("End of file reached when trying to determine read macro function.");
		}

		final int nextCodePoint = readResult.getResult();
		final FunctionStruct macroFunction = getMacroFunction(nextCodePoint);
		if (macroFunction == null) {
			throw new ReaderErrorException("No reader macro function exists for: " + codePoint + nextCodePoint + '.');
		}

		if (numberArgument.isPresent()) {
			return macroFunction.apply(
					inputStreamStruct,
					LispStructFactory.toCharacter(nextCodePoint),
					LispStructFactory.toInteger(numberArgument.get())
			);
		} else {
			return macroFunction.apply(
					inputStreamStruct,
					LispStructFactory.toCharacter(nextCodePoint),
					NILStruct.INSTANCE
			);
		}
//		return macroFunction.readMacro(inputStreamStruct, nextCodePoint, numberArgument);
	}

	/**
	 * Gets the {@link FunctionStruct} associated with the provided {@code codePoint}, or null if no such
	 * function exists.
	 *
	 * @param codePoint
	 * 		the code point associated with the {@link FunctionStruct} to retrieve
	 *
	 * @return the {@link FunctionStruct} associated with the provided {@code codePoint}, or null if no such
	 * function exists
	 */
	@Override
	public FunctionStruct getMacroFunction(final int codePoint) {
		return readerMacroFunctionMap.get(codePoint);
	}

	/**
	 * Sets the {@link FunctionStruct} with the provided {@code codePoint} to the provided {@code
	 * readerMacroFunction}.
	 *
	 * @param codePoint
	 * 		the code point associated with the {@link FunctionStruct} to set
	 * @param readerMacroFunction
	 * 		the new {@link FunctionStruct} to be associated
	 */
	@Override
	public void setMacroCharacter(final int codePoint, final FunctionStruct readerMacroFunction) {
		readerMacroFunctionMap.put(codePoint, readerMacroFunction);
	}

	@Override
	public boolean isDispatch() {
		return true;
	}
}
