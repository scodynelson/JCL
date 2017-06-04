package jcl.lang;

import java.util.function.Function;

import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.statics.CharacterConstants;
import jcl.util.CodePointConstants;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.sameInstance;

/**
 * Unit tests for {@link CharacterStruct} character methods.
 */
public class CharacterStructCharacterTest {

	/**
	 * {@link Rule} for performing expectations on exceptions.
	 */
	@Rule
	public final ExpectedException thrown = ExpectedException.none();

	/*
	Alpha-Char-P
	 */

	/**
	 * Test for {@link CharacterStruct#isAlphaChar()}.
	 */
	@Test
	public void test_isAlphaChar() {
		test_Character(CharacterStruct::isAlphaChar,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.T,
		               BooleanStruct.NIL,
		               BooleanStruct.T,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL);
	}

	/*
	Alphanumericp
	 */

	/**
	 * Test for {@link CharacterStruct#isAlphanumeric()}.
	 */
	@Test
	public void test_isAlphanumeric() {
		test_Character(CharacterStruct::isAlphanumeric,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.T,
		               BooleanStruct.NIL,
		               BooleanStruct.T,
		               BooleanStruct.NIL,
		               BooleanStruct.T,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL);
	}

	/*
	Digit-Char-P (not really)
	 */

	/**
	 * Test for {@link CharacterStruct#isDigitChar()}.
	 */
	@Test
	public void test_isDigitChar() {
		test_Character(CharacterStruct::isDigitChar,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.T,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL);
	}

	/*
	Graphic-Char-P
	 */

	/**
	 * Test for {@link CharacterStruct#isGraphicChar()}.
	 */
	@Test
	public void test_isGraphicChar() {
		test_Character(CharacterStruct::isGraphicChar,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.T,
		               BooleanStruct.T,
		               BooleanStruct.T,
		               BooleanStruct.T,
		               BooleanStruct.T,
		               BooleanStruct.T,
		               BooleanStruct.T,
		               BooleanStruct.NIL);
	}

	/*
	Standard-Char-P
	 */

	/**
	 * Test for {@link CharacterStruct#isStandardChar()}.
	 */
	@Test
	public void test_isStandardChar() {
		test_Character(CharacterStruct::isStandardChar,
		               BooleanStruct.NIL,
		               BooleanStruct.T,
		               BooleanStruct.NIL,
		               BooleanStruct.T,
		               BooleanStruct.T,
		               BooleanStruct.T,
		               BooleanStruct.T,
		               BooleanStruct.T,
		               BooleanStruct.T,
		               BooleanStruct.T,
		               BooleanStruct.NIL);
	}

	/*
	Char-Upcase
	 */

	/**
	 * Test for {@link CharacterStruct#charUpcase()} where the character is lower-cased.
	 */
	@Test
	public void test_charUpcase_Lower() {
		final CharacterStruct character = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct result = character.charUpcase();
		Assert.assertThat(result, is(CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#charUpcase()} where the character is upper-cased.
	 */
	@Test
	public void test_charUpcase_Upper() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct result = character.charUpcase();
		Assert.assertThat(result, is(CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#charUpcase()} where the character is non-alpha.
	 */
	@Test
	public void test_charUpcase_NonAlpha() {
		final CharacterStruct character = CharacterConstants.NUMBER_SIGN_CHAR;
		final CharacterStruct result = character.charUpcase();
		Assert.assertThat(result, is(CharacterConstants.NUMBER_SIGN_CHAR));
	}

	/*
	Char-Downcase
	 */

	/**
	 * Test for {@link CharacterStruct#charDowncase()} where the character is lower-cased.
	 */
	@Test
	public void test_charDowncase_Lower() {
		final CharacterStruct character = CharacterConstants.LATIN_SMALL_LETTER_A_CHAR;
		final CharacterStruct result = character.charDowncase();
		Assert.assertThat(result, is(CharacterConstants.LATIN_SMALL_LETTER_A_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#charDowncase()} where the character is upper-cased.
	 */
	@Test
	public void test_charDowncase_Upper() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final CharacterStruct result = character.charDowncase();
		Assert.assertThat(result, is(CharacterConstants.LATIN_SMALL_LETTER_A_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#charDowncase()} where the character is non-alpha.
	 */
	@Test
	public void test_charDowncase_NonAlpha() {
		final CharacterStruct character = CharacterConstants.NUMBER_SIGN_CHAR;
		final CharacterStruct result = character.charDowncase();
		Assert.assertThat(result, is(CharacterConstants.NUMBER_SIGN_CHAR));
	}

	/*
	Upper-Case-P
	 */

	/**
	 * Test for {@link CharacterStruct#isUpperCase()}.
	 */
	@Test
	public void test_isUpperCase() {
		test_Character(CharacterStruct::isUpperCase,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.T,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL);
	}

	/*
	Lower-Case-P
	 */

	/**
	 * Test for {@link CharacterStruct#isLowerCase()}.
	 */
	@Test
	public void test_isLowerCase() {
		test_Character(CharacterStruct::isLowerCase,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.T,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL);
	}

	/*
	Both-Case-P
	 */

	/**
	 * Test for {@link CharacterStruct#isBothCase()}.
	 */
	@Test
	public void test_isBothCase() {
		test_Character(CharacterStruct::isBothCase,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL,
		               BooleanStruct.T,
		               BooleanStruct.NIL,
		               BooleanStruct.T,
		               BooleanStruct.NIL,
		               BooleanStruct.NIL);
	}

	/*
	Char-Code
	 */

	/**
	 * Test for {@link CharacterStruct#charCode()}.
	 */
	@Test
	public void test_charCode() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final IntegerStruct result = character.charCode();
		Assert.assertThat(result, is(IntegerStruct.toLispInteger((int) CodePointConstants.LATIN_CAPITAL_LETTER_A)));
	}

	/*
	Code-Char
	 */

	/**
	 * Test for {@link CharacterStruct#codeChar(IntegerStruct)}.
	 */
	@Test
	public void test_codeChar() {
		final IntegerStruct code = IntegerStruct.toLispInteger((int) CodePointConstants.LATIN_CAPITAL_LETTER_A);
		final LispStruct result = CharacterStruct.codeChar(code);
		Assert.assertThat(result, is(CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#codeChar(IntegerStruct)} where the character is undefined.
	 */
	@Test
	public void test_codeChar_Undefined() {
		final IntegerStruct code = IntegerStruct.MINUS_ONE;
		final LispStruct result = CharacterStruct.codeChar(code);
		Assert.assertThat(result, is(NILStruct.INSTANCE));
	}

	/*
	Char-Name
	 */

	/**
	 * Test for {@link CharacterStruct#charName()}.
	 */
	@Test
	public void test_charName() {
		final CharacterStruct character = CharacterConstants.SPACE_CHAR;
		final StringStruct result = character.charName();
		Assert.assertThat(result.toJavaString(), is("SPACE"));
	}

	/*
	Char-Int
	 */

	/**
	 * Test for {@link CharacterStruct#charInt()}.
	 */
	@Test
	public void test_charInt() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final IntegerStruct result = character.charInt();
		Assert.assertThat(result, is(IntegerStruct.toLispInteger((int) CodePointConstants.LATIN_CAPITAL_LETTER_A)));
	}

	/*
	Char-Code
	 */

	/**
	 * Test for {@link CharacterStruct#charDigit()} where no radix is supplied.
	 */
	@Test
	public void test_charDigit_NoRadix() {
		final CharacterStruct character = CharacterConstants.DIGIT_ZERO_CHAR;
		final LispStruct result = character.charDigit();
		Assert.assertThat(result, is(IntegerStruct.ZERO));
	}

	/**
	 * Test for {@link CharacterStruct#charDigit()} where no radix is supplied and the character is not within the
	 * default radix.
	 */
	@Test
	public void test_charDigit_NoRadix_NotInRadix() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final LispStruct result = character.charDigit();
		Assert.assertThat(result, is(NILStruct.INSTANCE));
	}

	/**
	 * Test for {@link CharacterStruct#charDigit()} where a radix is supplied.
	 */
	@Test
	public void test_charDigit_Radix() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final LispStruct result = character.charDigit(IntegerStruct.toLispInteger(16));

		Assert.assertThat(result, is(IntegerStruct.TEN));
	}

	/**
	 * Test for {@link CharacterStruct#charDigit()} where a radix is supplied and the character is not within the
	 * supplied radix.
	 */
	@Test
	public void test_charDigit_NotInRadix() {
		final CharacterStruct character = CharacterConstants.DIGIT_EIGHT_CHAR;
		final LispStruct result = character.charDigit(IntegerStruct.TWO);
		Assert.assertThat(result, is(NILStruct.INSTANCE));
	}

	/*
	Digit-Char
	 */

	/**
	 * Test for {@link CharacterStruct#digitChar(IntegerStruct)} where no radix is supplied.
	 */
	@Test
	public void test_digitChar_NoRadix() {
		final LispStruct result = CharacterStruct.digitChar(IntegerStruct.ZERO);
		Assert.assertThat(result, is(CharacterConstants.DIGIT_ZERO_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#digitChar(IntegerStruct)} where no radix is supplied and the digit is not
	 * representable as a character within the default radix.
	 */
	@Test
	public void test_digitChar_NoRadix_WeightGreaterThanRadix() {
		final LispStruct result = CharacterStruct.digitChar(IntegerStruct.MINUS_ONE);
		Assert.assertThat(result, is(NILStruct.INSTANCE));
	}

	/**
	 * Test for {@link CharacterStruct#digitChar(IntegerStruct, IntegerStruct)} where a radix is supplied.
	 */
	@Test
	public void test_digitChar_Radix() {
		final IntegerStruct eleven = IntegerStruct.toLispInteger(11);
		final LispStruct result = CharacterStruct.digitChar(IntegerStruct.TEN, eleven);
		Assert.assertThat(result, is(CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#digitChar(IntegerStruct, IntegerStruct)} where a radix is supplied and the digit
	 * is not representable as a character within the supplied radix.
	 */
	@Test
	public void test_digitChar_Radix_WeightGreaterThanRadix() {
		final LispStruct result = CharacterStruct.digitChar(IntegerStruct.TWO, IntegerStruct.TWO);
		Assert.assertThat(result, is(NILStruct.INSTANCE));
	}

	/*
	Name-Char
	 */

	/**
	 * Test for {@link CharacterStruct#nameChar(LispStruct)} where the provided name value is an invalid type.
	 */
	@Test
	public void test_nameChar_TypeError() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage("Type cannot be converted to CHARACTER.");

		CharacterStruct.nameChar(IntegerStruct.ZERO);
	}

	/**
	 * Test for {@link CharacterStruct#nameChar(LispStruct)} where the provided name value is a {@link CharacterStruct}.
	 */
	@Test
	public void test_nameChar_Character() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final LispStruct result = CharacterStruct.nameChar(character);
		Assert.assertThat(result, is(character));
		Assert.assertThat(result, sameInstance(character));
	}

	/**
	 * Test for {@link CharacterStruct#nameChar(LispStruct)} where the provided name value is a {@link SymbolStruct}.
	 */
	@Test
	public void test_nameChar_Symbol() {
		final SymbolStruct symbol = LispStructFactory.toSymbol("SPACE");
		final LispStruct result = CharacterStruct.nameChar(symbol);
		Assert.assertThat(result, is(CharacterConstants.SPACE_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#nameChar(LispStruct)} where the provided name value is a {@link SymbolStruct} and
	 * is within the {@link CharacterConstants#CL_GRAPHIC_CHAR_NAME_MAP}.
	 */
	@Test
	public void test_nameChar_Symbol_CLGraphic() {
		final SymbolStruct symbol = LispStructFactory.toSymbol("^@");
		final LispStruct result = CharacterStruct.nameChar(symbol);
		Assert.assertThat(result, is(CharacterConstants.NULL_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#nameChar(LispStruct)} where the provided name value is a {@link SymbolStruct} and
	 * the character doesn't exist.
	 */
	@Test
	public void test_nameChar_Symbol_NoCharacterByName() {
		final SymbolStruct symbol = LispStructFactory.toSymbol("FOO");
		final LispStruct result = CharacterStruct.nameChar(symbol);
		Assert.assertThat(result, is(NILStruct.INSTANCE));
	}

	/**
	 * Test for {@link CharacterStruct#nameChar(LispStruct)} where the provided name value is a {@link StringStruct}.
	 */
	@Test
	public void test_nameChar_String() {
		final StringStruct string = StringStruct.toLispString("SPACE");
		final LispStruct result = CharacterStruct.nameChar(string);
		Assert.assertThat(result, is(CharacterConstants.SPACE_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#nameChar(LispStruct)} where the provided name value is a {@link StringStruct} and
	 * is within the {@link CharacterConstants#CL_GRAPHIC_CHAR_NAME_MAP}.
	 */
	@Test
	public void test_nameChar_String_CLGraphic() {
		final StringStruct string = StringStruct.toLispString("^@");
		final LispStruct result = CharacterStruct.nameChar(string);
		Assert.assertThat(result, is(CharacterConstants.NULL_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#nameChar(LispStruct)} where the provided name value is a {@link StringStruct} and
	 * the character doesn't exist.
	 */
	@Test
	public void test_nameChar_String_NoCharacterByName() {
		final StringStruct string = StringStruct.toLispString("FOO");
		final LispStruct result = CharacterStruct.nameChar(string);
		Assert.assertThat(result, is(NILStruct.INSTANCE));
	}

	/*
	To-Java-Char
	 */

	/**
	 * Test for {@link CharacterStruct#toJavaChar()}.
	 */
	@Test
	public void test_toJavaChar() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final char javaChar = character.toJavaChar();
		Assert.assertThat(javaChar, is('A'));
	}

	/*
	To-Java-Character
	 */

	/**
	 * Test for {@link CharacterStruct#toJavaCharacter()}.
	 */
	@Test
	public void test_toJavaCharacter() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final Character javaCharacter = character.toJavaCharacter();
		Assert.assertThat(javaCharacter, is(CodePointConstants.LATIN_CAPITAL_LETTER_A));
	}

	/*
	To-Unicode-Code-Point
	 */

	/**
	 * Test for {@link CharacterStruct#toUnicodeCodePoint()}.
	 */
	@Test
	public void test_toUnicodeCodePoint() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final int codePoint = character.toUnicodeCodePoint();
		Assert.assertThat(codePoint, is((int) CodePointConstants.LATIN_CAPITAL_LETTER_A));
	}

	/*
	To-Lisp-Character
	 */

	/**
	 * Test for {@link CharacterStruct#toLispCharacter(char)}.
	 */
	@Test
	public void test_toLispCharacter_char() {
		final CharacterStruct character = CharacterStruct.toLispCharacter('A');
		Assert.assertThat(character, is(CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#toLispCharacter(Character)}.
	 */
	@Test
	public void test_toLispCharacter_Character() {
		final CharacterStruct character = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_A);
		Assert.assertThat(character, is(CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#toLispCharacter(int)}.
	 */
	@Test
	public void test_toLispCharacter_codePoint_StandardChar() {
		final CharacterStruct character = CharacterStruct.toLispCharacter((int) 'A');
		Assert.assertThat(character, is(CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#toLispCharacter(int)} where the provided code-point is not a standard character.
	 */
	@Test
	public void test_toLispCharacter_codePoint_NotStandardChar() {
		final int codePoint = 0xABCD;
		final CharacterStruct character = CharacterStruct.toLispCharacter(codePoint);
		Assert.assertThat(character, notNullValue());
		Assert.assertThat(character.toUnicodeCodePoint(), is(codePoint));
	}

	/**
	 * Test for {@link CharacterStruct#toLispCharacter(LispStruct)} where the provided name value is an invalid type.
	 */
	@Test
	public void test_toLispCharacter_LispStruct_TypeError() {
		thrown.expect(TypeErrorException.class);
		thrown.expectMessage("Type cannot be converted to CHARACTER.");

		CharacterStruct.toLispCharacter(IntegerStruct.ZERO);
	}

	/**
	 * Test for {@link CharacterStruct#toLispCharacter(LispStruct)} where the provided name value is a {@link
	 * CharacterStruct}.
	 */
	@Test
	public void test_toLispCharacter_LispStruct_Character() {
		final CharacterStruct character = CharacterStruct.toLispCharacter(
				CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR);
		Assert.assertThat(character, is(CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#toLispCharacter(LispStruct)} where the provided name value is a {@link
	 * SymbolStruct}.
	 */
	@Test
	public void test_toLispCharacter_LispStruct_Symbol() {
		final SymbolStruct symbol = LispStructFactory.toSymbol("A");
		final LispStruct result = CharacterStruct.toLispCharacter(symbol);
		Assert.assertThat(result, is(CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#toLispCharacter(LispStruct)} where the provided name value is a {@link
	 * SymbolStruct} and has the wrong length.
	 */
	@Test
	public void test_toLispCharacter_LispStruct_Symbol_WrongLength() {
		thrown.expect(SimpleErrorException.class);
		thrown.expectMessage("Symbol name is not of length one: ");

		final SymbolStruct symbol = LispStructFactory.toSymbol("FOO");
		CharacterStruct.toLispCharacter(symbol);
	}

	/**
	 * Test for {@link CharacterStruct#toLispCharacter(LispStruct)} where the provided name value is a {@link
	 * StringStruct}.
	 */
	@Test
	public void test_toLispCharacter_LispStruct_String() {
		final StringStruct string = StringStruct.toLispString("A");
		final LispStruct result = CharacterStruct.toLispCharacter(string);
		Assert.assertThat(result, is(CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR));
	}

	/**
	 * Test for {@link CharacterStruct#toLispCharacter(LispStruct)} where the provided name value is a {@link
	 * StringStruct} and has the wrong length.
	 */
	@Test
	public void test_toLispCharacter_LispStruct_String_WrongLength() {
		thrown.expect(SimpleErrorException.class);
		thrown.expectMessage("String is not of length one: ");

		final StringStruct string = StringStruct.toLispString("FOO");
		CharacterStruct.toLispCharacter(string);
	}

	/*
	To-String
	 */

	/**
	 * Test for {@link CharacterStruct#toString()} where escaping is on.
	 */
	@Test
	public void test_toString_Escape() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;
		final String result = character.toString();
		Assert.assertThat(result, is("#\\A"));
	}

	/**
	 * Test for {@link CharacterStruct#toString()} where escaping is off.
	 */
	@Test
	public void test_toString_NoEscape() {
		final CharacterStruct character = CharacterConstants.LATIN_CAPITAL_LETTER_A_CHAR;

		ToStringTestUtils.validateToStringWithNoEscapes("A", character);
	}

	/*
	Private Helpers
	 */

	/**
	 * Tests all standard character code points, allowing for checking within ranges of code-points for validity.
	 *
	 * @param charFunction
	 * 		the character function to check
	 * @param preNewLineControl
	 * 		are characters pre-newline control characters
	 * @param newLine
	 * 		is newline character
	 * @param postNewLineControl
	 * 		are characters post-newline control characters
	 * @param preDigitSymbol
	 * 		are characters pre-digit symbol characters
	 * @param digit
	 * 		are characters digit characters
	 * @param preCapitalLetterSymbol
	 * 		are characters pre-capital-letter symbol characters
	 * @param capitalLetter
	 * 		are characters upper-cased letters
	 * @param postCapitalLetterSymbol
	 * 		are characters post-capital-letter symbol characters
	 * @param smallLetter
	 * 		are characters lower-cased letters
	 * @param postSmallLetterSymbol
	 * 		are characters post-small-letter symbol characters
	 * @param delete
	 * 		is delete character
	 */
	private static void test_Character(final Function<CharacterStruct, BooleanStruct> charFunction,
	                                   final BooleanStruct preNewLineControl,
	                                   final BooleanStruct newLine,
	                                   final BooleanStruct postNewLineControl,
	                                   final BooleanStruct preDigitSymbol,
	                                   final BooleanStruct digit,
	                                   final BooleanStruct preCapitalLetterSymbol,
	                                   final BooleanStruct capitalLetter,
	                                   final BooleanStruct postCapitalLetterSymbol,
	                                   final BooleanStruct smallLetter,
	                                   final BooleanStruct postSmallLetterSymbol,
	                                   final BooleanStruct delete) {
		for (int i = CodePointConstants.NULL; i < CodePointConstants.NEWLINE; i++) {
			final CharacterStruct character = CharacterStruct.toLispCharacter(i);
			final BooleanStruct result = charFunction.apply(character);
			Assert.assertThat("Character did not meet requirements: " + character, result, is(preNewLineControl));
		}
		{
			final CharacterStruct character = CharacterConstants.NEWLINE_CHAR;
			final BooleanStruct result = charFunction.apply(character);
			Assert.assertThat("Character did not meet requirements: " + character, result, is(newLine));
		}
		for (int i = CodePointConstants.VERTICAL_TAB; i < CodePointConstants.SPACE; i++) {
			final CharacterStruct character = CharacterStruct.toLispCharacter(i);
			final BooleanStruct result = charFunction.apply(character);
			Assert.assertThat("Character did not meet requirements: " + character, result, is(postNewLineControl));
		}
		for (int i = CodePointConstants.SPACE; i < CodePointConstants.DIGIT_ZERO; i++) {
			final CharacterStruct character = CharacterStruct.toLispCharacter(i);
			final BooleanStruct result = charFunction.apply(character);
			Assert.assertThat("Character did not meet requirements: " + character, result, is(preDigitSymbol));
		}
		for (int i = CodePointConstants.DIGIT_ZERO; i < CodePointConstants.COLON; i++) {
			final CharacterStruct character = CharacterStruct.toLispCharacter(i);
			final BooleanStruct result = charFunction.apply(character);
			Assert.assertThat("Character did not meet requirements: " + character, result, is(digit));
		}
		for (int i = CodePointConstants.COLON; i < CodePointConstants.LATIN_CAPITAL_LETTER_A; i++) {
			final CharacterStruct character = CharacterStruct.toLispCharacter(i);
			final BooleanStruct result = charFunction.apply(character);
			Assert.assertThat("Character did not meet requirements: " + character, result, is(preCapitalLetterSymbol));
		}
		for (int i = CodePointConstants.LATIN_CAPITAL_LETTER_A; i < CodePointConstants.LEFT_SQUARE_BRACKET; i++) {
			final CharacterStruct character = CharacterStruct.toLispCharacter(i);
			final BooleanStruct result = charFunction.apply(character);
			Assert.assertThat("Character did not meet requirements: " + character, result, is(capitalLetter));
		}
		for (int i = CodePointConstants.LEFT_SQUARE_BRACKET; i < CodePointConstants.LATIN_SMALL_LETTER_A; i++) {
			final CharacterStruct character = CharacterStruct.toLispCharacter(i);
			final BooleanStruct result = charFunction.apply(character);
			Assert.assertThat("Character did not meet requirements: " + character, result, is(postCapitalLetterSymbol));
		}
		for (int i = CodePointConstants.LATIN_SMALL_LETTER_A; i < CodePointConstants.LEFT_CURLY_BRACKET; i++) {
			final CharacterStruct character = CharacterStruct.toLispCharacter(i);
			final BooleanStruct result = charFunction.apply(character);
			Assert.assertThat("Character did not meet requirements: " + character, result, is(smallLetter));
		}
		for (int i = CodePointConstants.LEFT_CURLY_BRACKET; i < CodePointConstants.DELETE; i++) {
			final CharacterStruct character = CharacterStruct.toLispCharacter(i);
			final BooleanStruct result = charFunction.apply(character);
			Assert.assertThat("Character did not meet requirements: " + character, result, is(postSmallLetterSymbol));
		}
		final CharacterStruct character = CharacterConstants.DELETE_CHAR;
		final BooleanStruct result = charFunction.apply(character);
		Assert.assertThat("Character did not meet requirements: " + character, result, is(delete));
	}
}