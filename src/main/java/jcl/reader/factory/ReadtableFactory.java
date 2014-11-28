/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.factory;

import jcl.characters.CharacterConstants;
import jcl.reader.CaseSpec;
import jcl.reader.DispatchTable;
import jcl.reader.ReadtableStruct;
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

public final class ReadtableFactory {

	private ReadtableFactory() {
	}

	public static ReadtableStruct getInstance() {
		final ReadtableStruct readtable = new ReadtableStruct();
		return initializeReadtable(readtable);
	}

	public static ReadtableStruct getInstance(final CaseSpec caseSpec) {
		final ReadtableStruct readtable = new ReadtableStruct(caseSpec);
		return initializeReadtable(readtable);
	}

	private static ReadtableStruct initializeReadtable(final ReadtableStruct readtable) {

		//initialize the Standard-Readtable Reader Macro Functions
		readtable.setMacroCharacter(CharacterConstants.QUOTATION_MARK, QuotationMarkReaderMacroFunction.INSTANCE, false);
		readtable.setMacroCharacter(CharacterConstants.APOSTROPHE, ApostropheReaderMacroFunction.INSTANCE, false);
		readtable.setMacroCharacter(CharacterConstants.LEFT_PARENTHESIS, LeftParenthesisReaderMacroFunction.INSTANCE, false);
		readtable.setMacroCharacter(CharacterConstants.RIGHT_PARENTHESIS, RightParenthesisReaderMacroFunction.INSTANCE, false);
		readtable.setMacroCharacter(CharacterConstants.SEMICOLON, SemicolonReaderMacroFunction.INSTANCE, false);

		//initialize the Standard Sharp macro Functions
		readtable.makeDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, false);
		final DispatchTable dispatchTable = readtable.getDispatchTable(CharacterConstants.NUMBER_SIGN);

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

		return readtable;
	}
}
