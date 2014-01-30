package jcl.types;

import jcl.reader.macrofunction.impl.ApostropheReaderMacroFunction;
import jcl.reader.macrofunction.impl.LeftParenthesisReaderMacroFunction;
import jcl.reader.macrofunction.impl.QuotationMarkReaderMacroFunction;
import jcl.reader.macrofunction.impl.RightParenthesisReaderMacroFunction;
import jcl.reader.macrofunction.impl.SemicolonReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpAReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpApostropheReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpAsteriskReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpBReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpBackslashReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpCReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpColonReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpEqualsSignReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpFullStopReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpHyphenMinusReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpIllegalReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpLeftParenthesisReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpOReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpPReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpPlusSignReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpRReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpSReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpSharpReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpUReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpVerticalBarReaderMacroFunction;
import jcl.reader.macrofunction.impl.SharpXReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.packages.PackageStruct;
import jcl.structs.readtables.DispatchTable;
import jcl.structs.readtables.ReadtableStruct;
import jcl.structs.symbols.SymbolStruct;
import jcl.types.numbers.Float;
import jcl.types.numbers.SingleFloat;

import java.util.ArrayList;
import java.util.List;

public class Variable {

	public static final ReadtableStruct Readtable = ReadtableStruct.getStruct();
	public static int ReadBase = 10;
	public static boolean ReadSuppress;
	public static boolean ReadEval = true;
	public static final Float ReadDefaultFloatFormat = SingleFloat.INSTANCE;
	public static PackageStruct Package = PackageStruct.COMMON_LISP_USER;
	public static final List<SymbolStruct<?>> Features = new ArrayList<>();

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

	public static int getReadBase() {
		return ReadBase;
	}

	public static void setReadBase(final int readBase) {
		ReadBase = readBase;
	}

	public static boolean isReadSuppress() {
		return ReadSuppress;
	}

	public static void setReadSuppress(final boolean readSuppress) {
		ReadSuppress = readSuppress;
	}

	public static PackageStruct getPackage() {
		return Package;
	}

	public static void setPackage(final PackageStruct aPackage) {
		Package = aPackage;
	}
}
