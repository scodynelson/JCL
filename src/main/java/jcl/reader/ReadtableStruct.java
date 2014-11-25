package jcl.reader;

import jcl.characters.CharacterConstants;
import jcl.reader.macrofunction.ApostropheReaderMacroFunction;
import jcl.reader.macrofunction.LeftParenthesisReaderMacroFunction;
import jcl.reader.macrofunction.QuotationMarkReaderMacroFunction;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.macrofunction.RightParenthesisReaderMacroFunction;
import jcl.reader.macrofunction.SemicolonReaderMacroFunction;
import jcl.reader.macrofunction.SharpAReaderMacroFunction;
import jcl.reader.macrofunction.SharpApostropheReaderMacroFunction;
import jcl.reader.macrofunction.SharpAsteriskReaderMacroFunction;
import jcl.reader.macrofunction.SharpBReaderMacroFunction;
import jcl.reader.macrofunction.SharpBackslashReaderMacroFunction;
import jcl.reader.macrofunction.SharpCReaderMacroFunction;
import jcl.reader.macrofunction.SharpColonReaderMacroFunction;
import jcl.reader.macrofunction.SharpEqualsSignReaderMacroFunction;
import jcl.reader.macrofunction.SharpFullStopReaderMacroFunction;
import jcl.reader.macrofunction.SharpHyphenMinusReaderMacroFunction;
import jcl.reader.macrofunction.SharpIllegalReaderMacroFunction;
import jcl.reader.macrofunction.SharpLeftParenthesisReaderMacroFunction;
import jcl.reader.macrofunction.SharpOReaderMacroFunction;
import jcl.reader.macrofunction.SharpPReaderMacroFunction;
import jcl.reader.macrofunction.SharpPlusSignReaderMacroFunction;
import jcl.reader.macrofunction.SharpRReaderMacroFunction;
import jcl.reader.macrofunction.SharpSReaderMacroFunction;
import jcl.reader.macrofunction.SharpSharpReaderMacroFunction;
import jcl.reader.macrofunction.SharpUReaderMacroFunction;
import jcl.reader.macrofunction.SharpVerticalBarReaderMacroFunction;
import jcl.reader.macrofunction.SharpXReaderMacroFunction;
import jcl.classes.BuiltInClassStruct;
import jcl.types.Readtable;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

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
		//initialize the Standard-Readtable Reader Macro Functions
		setMacroCharacter(CharacterConstants.QUOTATION_MARK, QuotationMarkReaderMacroFunction.INSTANCE, false);
		setMacroCharacter(CharacterConstants.APOSTROPHE, ApostropheReaderMacroFunction.INSTANCE, false);
		setMacroCharacter(CharacterConstants.LEFT_PARENTHESIS, LeftParenthesisReaderMacroFunction.INSTANCE, false);
		setMacroCharacter(CharacterConstants.RIGHT_PARENTHESIS, RightParenthesisReaderMacroFunction.INSTANCE, false);
		setMacroCharacter(CharacterConstants.SEMICOLON, SemicolonReaderMacroFunction.INSTANCE, false);

		//initialize the Standard Sharp macro Functions
		makeDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, false);
		final DispatchTable dispatchTable = getDispatchTable(CharacterConstants.NUMBER_SIGN);

		dispatchTable.setMacroCharacter(CharacterConstants.BACKSLASH, SharpBackslashReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.APOSTROPHE, SharpApostropheReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LEFT_PARENTHESIS, SharpLeftParenthesisReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.ASTERISK, SharpAsteriskReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.COLON, SharpColonReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.FULL_STOP, SharpFullStopReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.VERTICAL_LINE, SharpVerticalBarReaderMacroFunction.INSTANCE);

		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_R, SharpRReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_R, SharpRReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_B, SharpBReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_B, SharpBReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_O, SharpOReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_O, SharpOReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_X, SharpXReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_X, SharpXReaderMacroFunction.INSTANCE);

		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_A, SharpAReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_A, SharpAReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_S, SharpSReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_S, SharpSReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_C, SharpCReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_C, SharpCReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_P, SharpPReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_P, SharpPReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_U, SharpUReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_U, SharpUReaderMacroFunction.INSTANCE);

		dispatchTable.setMacroCharacter(CharacterConstants.EQUALS_SIGN, SharpEqualsSignReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.NUMBER_SIGN, SharpSharpReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.PLUS_SIGN, SharpPlusSignReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.HYPHEN_MINUS, SharpHyphenMinusReaderMacroFunction.INSTANCE);

		dispatchTable.setMacroCharacter(CharacterConstants.RIGHT_PARENTHESIS, SharpIllegalReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LESS_THAN_SIGN, SharpIllegalReaderMacroFunction.INSTANCE);

		dispatchTable.setMacroCharacter(CharacterConstants.TAB, SharpIllegalReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.NEWLINE, SharpIllegalReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.LINE_FEED, SharpIllegalReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.SPACE, SharpIllegalReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.PAGE, SharpIllegalReaderMacroFunction.INSTANCE);
		dispatchTable.setMacroCharacter(CharacterConstants.RETURN, SharpIllegalReaderMacroFunction.INSTANCE);
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
	 * Gets the dispatching table for the provided {@code codePoint}.
	 *
	 * @param codePoint
	 * 		the key for the dispatch table
	 *
	 * @return the dispatching table for the provided {@code codePoint}
	 */
	public DispatchTable getDispatchTable(final int codePoint) {
		return dispatchTableMap.get(codePoint);
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
}
