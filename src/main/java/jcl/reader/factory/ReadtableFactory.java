/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.factory;

import jcl.characters.CharacterConstants;
import jcl.reader.CaseSpec;
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
import jcl.reader.struct.ReadtableStruct;

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

		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.BACKSLASH, SharpBackslashReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.APOSTROPHE, SharpApostropheReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LEFT_PARENTHESIS, SharpLeftParenthesisReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.ASTERISK, SharpAsteriskReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.COLON, SharpColonReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.FULL_STOP, SharpFullStopReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.VERTICAL_LINE, SharpVerticalBarReaderMacroFunction.INSTANCE);

		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_R, SharpRReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_R, SharpRReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_B, SharpBReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_B, SharpBReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_O, SharpOReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_O, SharpOReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_X, SharpXReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_X, SharpXReaderMacroFunction.INSTANCE);

		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_A, SharpAReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_A, SharpAReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_S, SharpSReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_S, SharpSReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_C, SharpCReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_C, SharpCReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_P, SharpPReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_P, SharpPReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_U, SharpUReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_U, SharpUReaderMacroFunction.INSTANCE);

		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.EQUALS_SIGN, SharpEqualsSignReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.NUMBER_SIGN, SharpSharpReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.PLUS_SIGN, SharpPlusSignReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.HYPHEN_MINUS, SharpHyphenMinusReaderMacroFunction.INSTANCE);

		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.RIGHT_PARENTHESIS, SharpIllegalReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LESS_THAN_SIGN, SharpIllegalReaderMacroFunction.INSTANCE);

		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.TAB, SharpIllegalReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.NEWLINE, SharpIllegalReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LINE_FEED, SharpIllegalReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.SPACE, SharpIllegalReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.PAGE, SharpIllegalReaderMacroFunction.INSTANCE);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.RETURN, SharpIllegalReaderMacroFunction.INSTANCE);

		return readtable;
	}
}
