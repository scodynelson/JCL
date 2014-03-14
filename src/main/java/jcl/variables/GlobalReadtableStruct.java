package jcl.variables;

import jcl.reader.macrofunction.ApostropheReaderMacroFunction;
import jcl.reader.macrofunction.LeftParenthesisReaderMacroFunction;
import jcl.reader.macrofunction.QuotationMarkReaderMacroFunction;
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
import jcl.syntax.CharacterConstants;
import jcl.structs.DispatchTable;
import jcl.structs.ReadtableStruct;

public final class GlobalReadtableStruct {

	public static final ReadtableStruct Readtable = new ReadtableStruct();

	static {
		//initialize the Standard-Readtable Reader Macro Functions
		Readtable.setMacroCharacter(CharacterConstants.QUOTATION_MARK, new QuotationMarkReaderMacroFunction(), false);
		Readtable.setMacroCharacter(CharacterConstants.APOSTROPHE, new ApostropheReaderMacroFunction(), false);
		Readtable.setMacroCharacter(CharacterConstants.LEFT_PARENTHESIS, new LeftParenthesisReaderMacroFunction(), false);
		Readtable.setMacroCharacter(CharacterConstants.RIGHT_PARENTHESIS, new RightParenthesisReaderMacroFunction(), false);
		Readtable.setMacroCharacter(CharacterConstants.SEMICOLON, new SemicolonReaderMacroFunction(), false);

		//initialize the Standard Sharp macro Functions
		Readtable.makeDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, false);
		final DispatchTable dispatchTable = Readtable.getDispatchTable(CharacterConstants.NUMBER_SIGN);

		dispatchTable.setMacroCharacter(CharacterConstants.BACKSLASH, new SharpBackslashReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.APOSTROPHE, new SharpApostropheReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LEFT_PARENTHESIS, new SharpLeftParenthesisReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.ASTERISK, new SharpAsteriskReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.COLON, new SharpColonReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.FULL_STOP, new SharpFullStopReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.VERTICAL_LINE, new SharpVerticalBarReaderMacroFunction());

		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_R, new SharpRReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_R, new SharpRReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_B, new SharpBReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_B, new SharpBReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_O, new SharpOReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_O, new SharpOReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_X, new SharpXReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_X, new SharpXReaderMacroFunction());

		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_A, new SharpAReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_A, new SharpAReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_S, new SharpSReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_S, new SharpSReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_C, new SharpCReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_C, new SharpCReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_P, new SharpPReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_P, new SharpPReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_CAPITAL_LETTER_U, new SharpUReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LATIN_SMALL_LETTER_U, new SharpUReaderMacroFunction());

		dispatchTable.setMacroCharacter(CharacterConstants.EQUALS_SIGN, new SharpEqualsSignReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.NUMBER_SIGN, new SharpSharpReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.PLUS_SIGN, new SharpPlusSignReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.HYPHEN_MINUS, new SharpHyphenMinusReaderMacroFunction());

		dispatchTable.setMacroCharacter(CharacterConstants.RIGHT_PARENTHESIS, new SharpIllegalReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LESS_THAN_SIGN, new SharpIllegalReaderMacroFunction());

		dispatchTable.setMacroCharacter(CharacterConstants.TAB, new SharpIllegalReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.NEWLINE, new SharpIllegalReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.LINE_FEED, new SharpIllegalReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.SPACE, new SharpIllegalReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.PAGE, new SharpIllegalReaderMacroFunction());
		dispatchTable.setMacroCharacter(CharacterConstants.RETURN, new SharpIllegalReaderMacroFunction());
	}
}
