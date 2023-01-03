package jcl.system;

import jcl.functions.reader.ApostropheReaderMacroFunction;
import jcl.functions.reader.BackquoteReaderMacroFunction;
import jcl.functions.reader.CommaReaderMacroFunction;
import jcl.functions.reader.LeftParenthesisReaderMacroFunction;
import jcl.functions.reader.QuotationMarkReaderMacroFunction;
import jcl.functions.reader.ReaderMacroFunctionImpl;
import jcl.functions.reader.RightParenthesisReaderMacroFunction;
import jcl.functions.reader.SemicolonReaderMacroFunction;
import jcl.functions.reader.SharpAReaderMacroFunction;
import jcl.functions.reader.SharpApostropheReaderMacroFunction;
import jcl.functions.reader.SharpAsteriskReaderMacroFunction;
import jcl.functions.reader.SharpBReaderMacroFunction;
import jcl.functions.reader.SharpBackslashReaderMacroFunction;
import jcl.functions.reader.SharpCReaderMacroFunction;
import jcl.functions.reader.SharpColonReaderMacroFunction;
import jcl.functions.reader.SharpEqualsSignReaderMacroFunction;
import jcl.functions.reader.SharpFullStopReaderMacroFunction;
import jcl.functions.reader.SharpHyphenMinusReaderMacroFunction;
import jcl.functions.reader.SharpIllegalReaderMacroFunction;
import jcl.functions.reader.SharpLeftParenthesisReaderMacroFunction;
import jcl.functions.reader.SharpOReaderMacroFunction;
import jcl.functions.reader.SharpPReaderMacroFunction;
import jcl.functions.reader.SharpPlusSignReaderMacroFunction;
import jcl.functions.reader.SharpRReaderMacroFunction;
import jcl.functions.reader.SharpSReaderMacroFunction;
import jcl.functions.reader.SharpSharpReaderMacroFunction;
import jcl.functions.reader.SharpUReaderMacroFunction;
import jcl.functions.reader.SharpVerticalBarReaderMacroFunction;
import jcl.functions.reader.SharpXReaderMacroFunction;
import jcl.lang.ReadtableStruct;
import jcl.lang.statics.CommonLispSymbols;
import jcl.util.CodePointConstants;
import lombok.experimental.UtilityClass;

@UtilityClass
public final class BootstrapReaderMacroFunctions {

	public static void bootstrap() {
		final ReadtableStruct readtable = CommonLispSymbols.READTABLE_VAR.getVariableValue();

		ReaderMacroFunctionImpl readerMacroFunction = new ApostropheReaderMacroFunction();
		readtable.setMacroCharacter(CodePointConstants.APOSTROPHE, readerMacroFunction, false);

		readerMacroFunction = new BackquoteReaderMacroFunction();
		readtable.setMacroCharacter(CodePointConstants.GRAVE_ACCENT, readerMacroFunction, false);

		readerMacroFunction = new CommaReaderMacroFunction();
		readtable.setMacroCharacter(CodePointConstants.COMMA, readerMacroFunction, false);

		readerMacroFunction = new LeftParenthesisReaderMacroFunction();
		readtable.setMacroCharacter(CodePointConstants.LEFT_PARENTHESIS, readerMacroFunction, false);

		readerMacroFunction = new QuotationMarkReaderMacroFunction();
		readtable.setMacroCharacter(CodePointConstants.QUOTATION_MARK, readerMacroFunction, false);

		readerMacroFunction = new RightParenthesisReaderMacroFunction();
		readtable.setMacroCharacter(CodePointConstants.RIGHT_PARENTHESIS, readerMacroFunction, false);

		readerMacroFunction = new SemicolonReaderMacroFunction();
		readtable.setMacroCharacter(CodePointConstants.SEMICOLON, readerMacroFunction, false);

		readerMacroFunction = new SharpApostropheReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.APOSTROPHE, readerMacroFunction);

		readerMacroFunction = new SharpAReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_A, readerMacroFunction);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_A, readerMacroFunction);

		readerMacroFunction = new SharpAsteriskReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.ASTERISK, readerMacroFunction);

		readerMacroFunction = new SharpBackslashReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.BACKSLASH, readerMacroFunction);

		readerMacroFunction = new SharpBReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_B, readerMacroFunction);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_B, readerMacroFunction);

		readerMacroFunction = new SharpColonReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.COLON, readerMacroFunction);

		readerMacroFunction = new SharpCReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_C, readerMacroFunction);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_C, readerMacroFunction);

		readerMacroFunction = new SharpEqualsSignReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.EQUALS_SIGN, readerMacroFunction);

		readerMacroFunction = new SharpFullStopReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.FULL_STOP, readerMacroFunction);

		readerMacroFunction = new SharpHyphenMinusReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.HYPHEN_MINUS, readerMacroFunction);

		readerMacroFunction = new SharpIllegalReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.RIGHT_PARENTHESIS, readerMacroFunction);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LESS_THAN_SIGN, readerMacroFunction);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.TAB, readerMacroFunction);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.NEWLINE, readerMacroFunction);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LINE_FEED, readerMacroFunction);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.SPACE, readerMacroFunction);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.PAGE, readerMacroFunction);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.RETURN, readerMacroFunction);

		readerMacroFunction = new SharpLeftParenthesisReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LEFT_PARENTHESIS, readerMacroFunction);

		readerMacroFunction = new SharpOReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_O, readerMacroFunction);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_O, readerMacroFunction);

		readerMacroFunction = new SharpPlusSignReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.PLUS_SIGN, readerMacroFunction);

		readerMacroFunction = new SharpPReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_P, readerMacroFunction);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_P, readerMacroFunction);

		readerMacroFunction = new SharpRReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_R, readerMacroFunction);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_R, readerMacroFunction);

		readerMacroFunction = new SharpSharpReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.NUMBER_SIGN, readerMacroFunction);

		readerMacroFunction = new SharpSReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_S, readerMacroFunction);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_S, readerMacroFunction);

		readerMacroFunction = new SharpUReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_U, readerMacroFunction);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_U, readerMacroFunction);

		readerMacroFunction = new SharpVerticalBarReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.VERTICAL_LINE, readerMacroFunction);

		readerMacroFunction = new SharpXReaderMacroFunction();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_X, readerMacroFunction);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_X, readerMacroFunction);
	}
}
