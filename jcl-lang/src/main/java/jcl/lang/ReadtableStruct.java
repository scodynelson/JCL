package jcl.lang;

import jcl.lang.readtable.AttributeType;
import jcl.lang.readtable.ReadtableCase;
import jcl.lang.readtable.SyntaxType;

/**
 * The {@link ReadtableStruct} is the object representation of a Lisp 'readtable' type.
 */
public interface ReadtableStruct extends LispStruct {

	/**
	 * Creates a new dispatching table for the provided {@code codePoint}, designating the {@link SyntaxType} as
	 * terminating if the provided {@code nonTerminatingP} is false.
	 *
	 * @param codePoint
	 * 		the key for the new dispatching table
	 * @param nonTerminatingP
	 * 		true if the character should be non-terminating; false otherwise
	 *
	 * @return the value of {@code nonTerminatingP}
	 */
	boolean makeDispatchMacroCharacter(final FunctionStruct dispatchTable, final int codePoint, final boolean nonTerminatingP);

	/**
	 * Sets the {@link FunctionStruct} for the provided {@code codePoint} to the provided {@code
	 * readerMacroFunction}, designating the {@link SyntaxType} as terminating if the provided {@code nonTerminatingP}
	 * is false.
	 *
	 * @param codePoint
	 * 		the key for the {@link FunctionStruct} to set
	 * @param readerMacroFunction
	 * 		the new {@link FunctionStruct}
	 * @param nonTerminatingP
	 * 		true if the character should be non-terminating; false otherwise
	 */
	void setMacroCharacter(final int codePoint, final FunctionStruct readerMacroFunction, final boolean nonTerminatingP);

	/**
	 * Getter for readtable {@link ReadtableCase} property.
	 *
	 * @return readtable {@link ReadtableCase} property
	 */
	ReadtableCase getReadtableCase();

	/**
	 * Setter for readtable {@link ReadtableCase} property.
	 *
	 * @param readtableCase
	 * 		new readtable {@link ReadtableCase} property value
	 */
	void setReadtableCase(final ReadtableCase readtableCase);

	/**
	 * Retrieves the {@link FunctionStruct} for the provided {@code codePoint}.
	 *
	 * @param codePoint
	 * 		the key for the {@link FunctionStruct}
	 *
	 * @return the {@link FunctionStruct} for the provided {@code codePoint}
	 */
	FunctionStruct getMacroCharacter(final int codePoint);

	/**
	 * Gets the {@link FunctionStruct} for the provided {@code subCodePoint} within the provided {@code
	 * dispatchCodePoint}'s dispatching table.
	 *
	 * @param dispatchCodePoint
	 * 		the key for the dispatching table to search for the {@link FunctionStruct}
	 * @param subCodePoint
	 * 		the key for the {@link FunctionStruct}
	 *
	 * @return the {@link FunctionStruct} for the provided {@code subCodePoint}
	 */
	FunctionStruct getDispatchMacroCharacter(final int dispatchCodePoint, final int subCodePoint);

	/**
	 * Sets the {@link FunctionStruct} for the provided {@code subCodePoint} to the provided {@code
	 * readerMacroFunction} within the provided {@code dispatchCodePoint}'s dispatching table.
	 *
	 * @param dispatchCodePoint
	 * 		the key for the dispatching table to set the {@link FunctionStruct}
	 * @param subCodePoint
	 * 		the key for the {@link FunctionStruct} to set
	 * @param readerMacroFunction
	 * 		the new {@link FunctionStruct}
	 */
	void setDispatchMacroCharacter(final int dispatchCodePoint, final int subCodePoint, final FunctionStruct readerMacroFunction);

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
	AttributeType getAttributeType(final int codePoint, final IntegerStruct readBase);

	/**
	 * Gets the {@link SyntaxType} for the provided {@code codePoint} value.
	 *
	 * @param codePoint
	 * 		the codePoint for the {@link SyntaxType} to retrieve
	 *
	 * @return the {@link SyntaxType} for the provided {@code codePoint} value
	 */
	SyntaxType getSyntaxType(final int codePoint);
}
