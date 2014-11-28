package jcl.reader.struct;

import jcl.LispStruct;
import jcl.classes.BuiltInClassStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.AttributeType;
import jcl.reader.CaseSpec;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.SyntaxType;
import jcl.streams.ReadPeekResult;
import jcl.types.Readtable;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.math.BigInteger;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The {@link ReadtableStruct} is the object representation of a Lisp 'readtable' type.
 */
public class ReadtableStruct extends BuiltInClassStruct {

	private final Map<Integer, ReaderMacroFunction> macroTableMap = new ConcurrentHashMap<>();
	private final Map<Integer, DispatchTable> dispatchTableMap = new ConcurrentHashMap<>();

	private final AttributeTable attributeTable = new AttributeTable();
	private final SyntaxTable syntaxTable = new SyntaxTable();

	private CaseSpec readtableCase;

	/**
	 * Public constructor.
	 */
	public ReadtableStruct() {
		this(CaseSpec.UPCASE);
	}

	/**
	 * Public constructor.
	 *
	 * @param readtableCase
	 * 		the readtable case
	 */
	public ReadtableStruct(final CaseSpec readtableCase) {
		super(Readtable.INSTANCE, null, null);
		this.readtableCase = readtableCase;

		init();
	}

	/**
	 * Initializes and fills all standard readtable structures.
	 */
	private void init() {
//		ReaderMacroFunctionImpl.initializeReadtable(this);
	}

	/**
	 * Getter for readtable {@link #readtableCase} property.
	 *
	 * @return readtable {@link #readtableCase} property
	 */
	public CaseSpec getReadtableCase() {
		return readtableCase;
	}

	/**
	 * Setter for readtable {@link #readtableCase} property.
	 *
	 * @param readtableCase
	 * 		new readtable {@link #readtableCase} property
	 */
	public void setReadtableCase(final CaseSpec readtableCase) {
		this.readtableCase = readtableCase;
	}

	/**
	 * Retrieves the macro character reader function for the provided {@code codePoint}.
	 *
	 * @param codePoint
	 * 		the key for the macro character reader function
	 *
	 * @return the macro character reader function for the provided {@code codePoint}
	 */
	public ReaderMacroFunction getMacroCharacter(final int codePoint) {
		return macroTableMap.get(codePoint);
	}

	/**
	 * Sets the macro character reader function for the provided {@code codePoint} to the provided {@code
	 * readerMacroFunction}, designating the syntax type as terminating if the provided {@code nonTerminatingP} is
	 * false.
	 *
	 * @param codePoint
	 * 		the key for the macro character reader function to set
	 * @param readerMacroFunction
	 * 		the new macro character reader function
	 * @param nonTerminatingP
	 * 		true if the character should be non-terminating; false otherwise
	 */
	public void setMacroCharacter(final int codePoint, final ReaderMacroFunction readerMacroFunction, final boolean nonTerminatingP) {
		if (!nonTerminatingP) {
			syntaxTable.setSyntaxType(codePoint, SyntaxType.TERMINATING);
		}
		macroTableMap.put(codePoint, readerMacroFunction);
	}

	/**
	 * Creates a dispatching table for the provided {@code codePoint}, designating the syntax type as terminating if
	 * the provided {@code nonTerminatingP} is false.
	 *
	 * @param codePoint
	 * 		the key for the new dispatch table
	 * @param nonTerminatingP
	 * 		true if the character should be non-terminating; false otherwise
	 *
	 * @return the value of {@code nonTerminatingP}
	 */
	public boolean makeDispatchMacroCharacter(final int codePoint, final boolean nonTerminatingP) {
		final DispatchTable dispatchTable = new DispatchTable();
		setMacroCharacter(codePoint, dispatchTable, nonTerminatingP);
		dispatchTableMap.put(codePoint, dispatchTable);
		return nonTerminatingP;
	}

	/**
	 * Gets the macro character reader function for the provided {@code subCodePoint} within the provided {@code
	 * dispatchCodePoint} dispatch table.
	 *
	 * @param dispatchCodePoint
	 * 		the key for the dispatch table to search for the macro character reader function
	 * @param subCodePoint
	 * 		the key for the macro character reader function
	 *
	 * @return the macro character reader function for the provided {@code subCodePoint}
	 */
	public ReaderMacroFunction getDispatchMacroCharacter(final int dispatchCodePoint, final int subCodePoint) {
		return dispatchTableMap.get(dispatchCodePoint).getMacroFunction(subCodePoint);
	}

	/**
	 * Sets the macro character reader function for the provided {@code subCodePoint} to the provided {@code
	 * readerMacroFunction} within the provided {@code dispatchCodePoint} dispatch table.
	 *
	 * @param dispatchCodePoint
	 * 		the key for the dispatch table to set the macro character reader function
	 * @param subCodePoint
	 * 		the key for the macro character reader function to set
	 * @param readerMacroFunction
	 * 		the new macro character reader function
	 */
	public void setDispatchMacroCharacter(final int dispatchCodePoint, final int subCodePoint, final ReaderMacroFunction readerMacroFunction) {
		dispatchTableMap.get(dispatchCodePoint).setMacroCharacter(subCodePoint, readerMacroFunction);
	}

	/**
	 * Gets the attribute type for the provided {@code codePoint} value.
	 *
	 * @param codePoint
	 * 		the codePoint for the attribute type to retrieve
	 *
	 * @return the attribute type for the provided {@code codePoint} value
	 */
	public AttributeType getAttributeType(final int codePoint) {
		return attributeTable.getAttribute(codePoint);
	}

	/**
	 * Gets the syntax type for the provided {@code codePoint} value.
	 *
	 * @param codePoint
	 * 		the codePoint for the syntax type to retrieve
	 *
	 * @return the syntax type for the provided {@code codePoint} value
	 */
	public SyntaxType getSyntaxType(final int codePoint) {
		return syntaxTable.getSyntaxType(codePoint);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}

	/**
	 * This holds mappings for code points to macro functions and delegates to the proper {@link ReaderMacroFunction}
	 * when used.
	 */
	private static final class DispatchTable implements ReaderMacroFunction {

		/**
		 * The internal mapping of character code points to {@link ReaderMacroFunction}s to dispatch on when reading.
		 */
		private final Map<Integer, ReaderMacroFunction> macroFunctionMap = new ConcurrentHashMap<>();

		@Override
		public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {

			final ReadPeekResult readResult = reader.readChar(false, null, false);
			if (readResult.isEof()) {
				throw new ReaderErrorException("End of file reached when trying to determine read macro function.");
			}

			final int readChar = readResult.getResult();
			final ReaderMacroFunction macroFunction = getMacroFunction(readChar);
			if (macroFunction == null) {
				throw new ReaderErrorException("No reader macro function exists for: " + codePoint + readChar + '.');
			}

			return macroFunction.readMacro(readChar, reader, numArg);
		}

		@Override
		public boolean isDispatch() {
			return true;
		}

		/**
		 * Gets the macro function associated with the provided {@code codePoint}, or null if no such function exists.
		 *
		 * @param codePoint
		 * 		the code point associated with the macro function to retrieve
		 *
		 * @return the macro function associated with the provided {@code codePoint}, or null if no such function exists
		 */
		private ReaderMacroFunction getMacroFunction(final int codePoint) {
			return macroFunctionMap.get(codePoint);
		}

		/**
		 * Sets the macro function with the provided {@code codePoint} to the provided {@link ReaderMacroFunction}.
		 *
		 * @param codePoint
		 * 		the code point associated with the macro function to set
		 * @param macroFunction
		 * 		the new macro function to be associated
		 */
		private void setMacroCharacter(final int codePoint, final ReaderMacroFunction macroFunction) {
			macroFunctionMap.put(codePoint, macroFunction);
		}

		@Override
		public String toString() {
			return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
		}
	}
}
