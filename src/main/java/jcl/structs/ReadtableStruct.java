package jcl.structs;

import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.AttributeType;
import jcl.reader.syntax.CaseSpec;
import jcl.reader.syntax.SyntaxType;
import jcl.types.Readtable;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The {@code ReadtableStruct} is the object representation of a Lisp 'readtable' type.
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
	 * @param readtableCase the readtable case
	 */
	public ReadtableStruct(final CaseSpec readtableCase) {
		super(Readtable.INSTANCE, null, null);
		this.readtableCase = readtableCase;
	}

	/**
	 * Getter for readtable case property.
	 *
	 * @return readtable case property
	 */
	public CaseSpec getReadtableCase() {
		return readtableCase;
	}

	/**
	 * Setter for readtable case property.
	 *
	 * @param readtableCase new readtable case property
	 */
	public void setReadtableCase(final CaseSpec readtableCase) {
		this.readtableCase = readtableCase;
	}

	/**
	 * This method retrieves the macro character reader function for the provided {@code codePoint}.
	 *
	 * @param codePoint the key for the macro character reader function
	 * @return the macro character reader function for the provided {@code codePoint}
	 */
	public ReaderMacroFunction getMacroCharacter(final int codePoint) {
		return macroTableMap.get(codePoint);
	}

	/**
	 * This method sets the macro character reader function for the provided {@code codePoint} to the provided {@code readerMacroFunction},
	 * designating the syntax type as terminating if the provided {@code nonTerminatingP} is false.
	 *
	 * @param codePoint           the key for the macro character reader function to set
	 * @param readerMacroFunction the new macro character reader function
	 * @param nonTerminatingP     true if the character should be non-terminating; false otherwise
	 */
	public void setMacroCharacter(final int codePoint, final ReaderMacroFunction readerMacroFunction, final boolean nonTerminatingP) {
		if (!nonTerminatingP) {
			syntaxTable.setSyntaxType(codePoint, SyntaxType.TERMINATING);
		}
		macroTableMap.put(codePoint, readerMacroFunction);
	}

	/**
	 * This method gets the dispatching table for the provided {@code codePoint}.
	 *
	 * @param codePoint the key for the dispatch table
	 * @return the dispatching table for the provided {@code codePoint}
	 */
	public DispatchTable getDispatchTable(final int codePoint) {
		return dispatchTableMap.get(codePoint);
	}

	/**
	 * This method creates a dispatching table for the provided {@code codePoint}, designating the syntax type as terminating
	 * if the provided {@code nonTerminatingP} is false.
	 *
	 * @param codePoint       the key for the new dispatch table
	 * @param nonTerminatingP true if the character should be non-terminating; false otherwise
	 * @return the value of {@code nonTerminatingP}
	 */
	public boolean makeDispatchMacroCharacter(final int codePoint, final boolean nonTerminatingP) {
		final DispatchTable dispatchTable = new DispatchTable();
		setMacroCharacter(codePoint, dispatchTable, nonTerminatingP);
		dispatchTableMap.put(codePoint, dispatchTable);
		return nonTerminatingP;
	}

	/**
	 * This method gets the macro character reader function for the provided {@code subCodePoint} within the provided
	 * {@code dispatchCodePoint} dispatch table.
	 *
	 * @param dispatchCodePoint the key for the dispatch table to search for the macro character reader function
	 * @param subCodePoint      the key for the macro character reader function
	 * @return the macro character reader function for the provided {@code subCodePoint}
	 */
	public ReaderMacroFunction getDispatchMacroCharacter(final int dispatchCodePoint, final int subCodePoint) {
		return dispatchTableMap.get(dispatchCodePoint).getMacroCharacter(subCodePoint);
	}

	/**
	 * This method sets the macro character reader function for the provided {@code subCodePoint} to the provided {@code readerMacroFunction}
	 * within the provided {@code dispatchCodePoint} dispatch table.
	 *
	 * @param dispatchCodePoint   the key for the dispatch table to set the macro character reader function
	 * @param subCodePoint        the key for the macro character reader function to set
	 * @param readerMacroFunction the new macro character reader function
	 */
	public void setDispatchMacroCharacter(final int dispatchCodePoint, final int subCodePoint, final ReaderMacroFunction readerMacroFunction) {
		dispatchTableMap.get(dispatchCodePoint).setMacroCharacter(subCodePoint, readerMacroFunction);
	}

	/**
	 * This method gets the attribute type for the provided {@code codePoint} value.
	 *
	 * @param codePoint the codePoint for the attribute type to retrieve
	 * @return the attribute type for the provided {@code codePoint} value
	 */
	public AttributeType getAttributeType(final int codePoint) {
		return attributeTable.getAttribute(codePoint);
	}

	/**
	 * This method gets the syntax type for the provided {@code codePoint} value.
	 *
	 * @param codePoint the codePoint for the syntax type to retrieve
	 * @return the syntax type for the provided {@code codePoint} value
	 */
	public SyntaxType getSyntaxType(final int codePoint) {
		return syntaxTable.getSyntaxType(codePoint);
	}
}
