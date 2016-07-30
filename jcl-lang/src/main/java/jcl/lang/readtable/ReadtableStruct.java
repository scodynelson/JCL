/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.readtable;

import java.math.BigInteger;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.BuiltInClassStruct;
import jcl.lang.LispStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.function.ReaderMacroFunction;
import jcl.lang.number.IntegerStruct;
import jcl.lang.stream.ReadPeekResult;
import jcl.type.ReadtableType;
import jcl.util.CodePointConstants;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link ReadtableStruct} is the object representation of a Lisp 'readtable' type.
 */
public class ReadtableStruct extends BuiltInClassStruct {

	/**
	 * Internal map storing the {@link Integer} code point mappings to appropriate {@link ReaderMacroFunction}s.
	 */
	private final Map<Integer, ReaderMacroFunction> macroTableMap = new ConcurrentHashMap<>();

	/**
	 * Internal map storing the {@link Integer} code point mappings to appropriate {@link DispatchTable}s for
	 * dispatching on specializing {@link ReaderMacroFunction}s.
	 */
	private final Map<Integer, DispatchTable> dispatchTableMap = new ConcurrentHashMap<>();

	/**
	 * Internal {@link AttributeTable} storing the {@link Integer} code point mappings to {@link AttributeType}s.
	 */
	private final AttributeTable attributeTable = new AttributeTable();

	/**
	 * Internal {@link SyntaxTable} storing the {@link Integer} code point mappings to {@link SyntaxType}s.
	 */
	private final SyntaxTable syntaxTable = new SyntaxTable();

	/**
	 * The readtable case.
	 */
	private ReadtableCase readtableCase;

	/**
	 * Public constructor.
	 */
	private ReadtableStruct() {
		this(ReadtableCase.UPCASE);
	}

	/**
	 * Public constructor.
	 *
	 * @param readtableCase
	 * 		the readtable case
	 */
	private ReadtableStruct(final ReadtableCase readtableCase) {
		super(ReadtableType.INSTANCE, null, null);
		this.readtableCase = readtableCase;

		init();
	}

	public static ReadtableStruct valueOf() {
		return new ReadtableStruct();
	}

	public static ReadtableStruct valueOf(final ReadtableCase readtableCase) {
		return new ReadtableStruct(readtableCase);
	}

	/**
	 * Initializes and fills all standard readtable structures.
	 */
	private void init() {
		// Initialize '#' DispatchTable
		makeDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, false);
	}

	/**
	 * Creates a new {@link DispatchTable} for the provided {@code codePoint}, designating the {@link SyntaxType} as
	 * terminating if the provided {@code nonTerminatingP} is false.
	 *
	 * @param codePoint
	 * 		the key for the new {@link DispatchTable}
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
	 * Sets the {@link ReaderMacroFunction} for the provided {@code codePoint} to the provided {@code
	 * readerMacroFunction}, designating the {@link SyntaxType} as terminating if the provided {@code nonTerminatingP}
	 * is false.
	 *
	 * @param codePoint
	 * 		the key for the {@link ReaderMacroFunction} to set
	 * @param readerMacroFunction
	 * 		the new {@link ReaderMacroFunction}
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
	 * Getter for readtable {@link #readtableCase} property.
	 *
	 * @return readtable {@link #readtableCase} property
	 */
	public ReadtableCase getReadtableCase() {
		return readtableCase;
	}

	/**
	 * Setter for readtable {@link #readtableCase} property.
	 *
	 * @param readtableCase
	 * 		new readtable {@link #readtableCase} property value
	 */
	public void setReadtableCase(final ReadtableCase readtableCase) {
		this.readtableCase = readtableCase;
	}

	/**
	 * Retrieves the {@link ReaderMacroFunction} for the provided {@code codePoint}.
	 *
	 * @param codePoint
	 * 		the key for the {@link ReaderMacroFunction}
	 *
	 * @return the {@link ReaderMacroFunction} for the provided {@code codePoint}
	 */
	public ReaderMacroFunction getMacroCharacter(final int codePoint) {
		return macroTableMap.get(codePoint);
	}

	/**
	 * Gets the {@link ReaderMacroFunction} for the provided {@code subCodePoint} within the provided {@code
	 * dispatchCodePoint}'s {@link DispatchTable}.
	 *
	 * @param dispatchCodePoint
	 * 		the key for the {@link DispatchTable} to search for the {@link ReaderMacroFunction}
	 * @param subCodePoint
	 * 		the key for the {@link ReaderMacroFunction}
	 *
	 * @return the {@link ReaderMacroFunction} for the provided {@code subCodePoint}
	 */
	public ReaderMacroFunction getDispatchMacroCharacter(final int dispatchCodePoint, final int subCodePoint) {
		return dispatchTableMap.get(dispatchCodePoint).getMacroFunction(subCodePoint);
	}

	/**
	 * Sets the {@link ReaderMacroFunction} for the provided {@code subCodePoint} to the provided {@code
	 * readerMacroFunction} within the provided {@code dispatchCodePoint}'s {@link DispatchTable}.
	 *
	 * @param dispatchCodePoint
	 * 		the key for the {@link DispatchTable} to set the {@link ReaderMacroFunction}
	 * @param subCodePoint
	 * 		the key for the {@link ReaderMacroFunction} to set
	 * @param readerMacroFunction
	 * 		the new {@link ReaderMacroFunction}
	 */
	public void setDispatchMacroCharacter(final int dispatchCodePoint, final int subCodePoint, final ReaderMacroFunction readerMacroFunction) {
		dispatchTableMap.get(dispatchCodePoint).setMacroCharacter(subCodePoint, readerMacroFunction);
	}

	/**
	 * Gets the {@link AttributeType} for the provided {@code codePoint} value.
	 *
	 * @param codePoint
	 * 		the codePoint for the {@link AttributeType} to retrieve
	 * @param readBase
	 * 		the read-base valued used to retrieve the appropriate {@link AttributeType}
	 *
	 * @return the {@link AttributeType} for the provided {@code codePoint} value
	 */
	public AttributeType getAttributeType(final int codePoint, final IntegerStruct readBase) {
		return attributeTable.getAttribute(codePoint, readBase);
	}

	/**
	 * Gets the {@link SyntaxType} for the provided {@code codePoint} value.
	 *
	 * @param codePoint
	 * 		the codePoint for the {@link SyntaxType} to retrieve
	 *
	 * @return the {@link SyntaxType} for the provided {@code codePoint} value
	 */
	public SyntaxType getSyntaxType(final int codePoint) {
		return syntaxTable.getSyntaxType(codePoint);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(macroTableMap)
		                            .append(dispatchTableMap)
		                            .append(attributeTable)
		                            .append(syntaxTable)
		                            .append(readtableCase)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final ReadtableStruct rhs = (ReadtableStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(macroTableMap, rhs.macroTableMap)
		                          .append(dispatchTableMap, rhs.dispatchTableMap)
		                          .append(attributeTable, rhs.attributeTable)
		                          .append(syntaxTable, rhs.syntaxTable)
		                          .append(readtableCase, rhs.readtableCase)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(macroTableMap)
		                                                                .append(dispatchTableMap)
		                                                                .append(attributeTable)
		                                                                .append(syntaxTable)
		                                                                .append(readtableCase)
		                                                                .toString();
	}

	/**
	 * This holds mappings for code points to {@link ReaderMacroFunction}s and delegates to the proper one when used.
	 */
	private static final class DispatchTable extends ReaderMacroFunction {

		/**
		 * Internal map storing character code points to {@link ReaderMacroFunction}s to dispatch on when reading.
		 */
		private final Map<Integer, ReaderMacroFunction> readerMacroFunctionMap = new ConcurrentHashMap<>();

		@Override
		public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {

			final ReadPeekResult readResult = reader.readChar(false, null, false);
			if (readResult.isEof()) {
				throw new ReaderErrorException("End of file reached when trying to determine read macro function.");
			}

			final int nextCodePoint = readResult.getResult();
			final ReaderMacroFunction macroFunction = getMacroFunction(nextCodePoint);
			if (macroFunction == null) {
				throw new ReaderErrorException("No reader macro function exists for: " + codePoint + nextCodePoint + '.');
			}

			return macroFunction.readMacro(nextCodePoint, reader, numberArgument);
		}

		@Override
		public boolean isDispatch() {
			return true;
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
		private ReaderMacroFunction getMacroFunction(final int codePoint) {
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
		private void setMacroCharacter(final int codePoint, final ReaderMacroFunction readerMacroFunction) {
			readerMacroFunctionMap.put(codePoint, readerMacroFunction);
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder().appendSuper(super.hashCode())
			                            .append(readerMacroFunctionMap)
			                            .toHashCode();
		}

		@Override
		public boolean equals(final Object obj) {
			if (obj == null) {
				return false;
			}
			if (obj == this) {
				return true;
			}
			if (obj.getClass() != getClass()) {
				return false;
			}
			final DispatchTable rhs = (DispatchTable) obj;
			return new EqualsBuilder().appendSuper(super.equals(obj))
			                          .append(readerMacroFunctionMap, rhs.readerMacroFunctionMap)
			                          .isEquals();
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(readerMacroFunctionMap)
			                                                                .toString();
		}
	}
}
