package jcl.lang;

import java.util.Map;

import com.ibm.icu.lang.UCharacter;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.CharacterStructImpl;
import jcl.lang.statics.CharacterConstants;

/**
 * The {@link CharacterStruct} is the object representation of a Lisp 'character' type.
 */
public interface CharacterStruct extends LispStruct {

	/**
	 * Returns the '=' comparison of this CharacterStruct and the provided CharacterStruct.
	 *
	 * @param character
	 * 		the CharacterStruct to be used in the '=' operation
	 *
	 * @return the '=' comparison of this CharacterStruct and the provided CharacterStruct
	 */
	boolean isEqualTo(final CharacterStruct character);

	/**
	 * Returns the '=' comparison of the provided CharacterStructs. If at any point a value does not follow the expected
	 * comparison, the comparison loop with short-circuit.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the '=' operation
	 *
	 * @return the '=' comparison of the provided CharacterStructs
	 */
	static BooleanStruct isEqualTo(final CharacterStruct... characters) {
		if (characters.length == 0) {
			throw new ErrorException("At least one character required to test equality.");
		}

		CharacterStruct previousCharacter = characters[0];

		boolean result = true;
		for (int i = 1; i < characters.length; i++) {
			final CharacterStruct currentCharacter = characters[i];
			result = previousCharacter.isEqualTo(currentCharacter);
			if (!result) {
				break;
			}
			previousCharacter = currentCharacter;
		}
		return BooleanStruct.toLispBoolean(result);
	}

	/**
	 * Returns the '!=' comparison of this CharacterStruct and the provided CharacterStruct.
	 *
	 * @param character
	 * 		the CharacterStruct to be used in the '!=' operation
	 *
	 * @return the '!=' comparison of this CharacterStruct and the provided CharacterStruct
	 */
	boolean isNotEqualTo(final CharacterStruct character);

	/**
	 * Returns the '!=' comparison of the provided CharacterStructs. If at any point a value does not follow the
	 * expected comparison, the comparison loop with short-circuit.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the '!=' operation
	 *
	 * @return the '!=' comparison of the provided CharacterStructs
	 */
	static BooleanStruct isNotEqualTo(final CharacterStruct... characters) {
		if (characters.length == 0) {
			throw new ErrorException("At least one character required to test equality.");
		}

		CharacterStruct previousCharacter = characters[0];

		boolean result = true;
		for (int i = 1; i < characters.length; i++) {
			final CharacterStruct currentCharacter = characters[i];
			result = previousCharacter.isNotEqualTo(currentCharacter);
			if (!result) {
				break;
			}
			previousCharacter = currentCharacter;
		}
		return BooleanStruct.toLispBoolean(result);
	}

	/**
	 * Returns the {@literal '<'} comparison of this CharacterStruct and the provided CharacterStruct.
	 *
	 * @param character
	 * 		the CharacterStruct to be used in the {@literal '<'} operation
	 *
	 * @return the {@literal '<'} comparison of this CharacterStruct and the provided CharacterStruct
	 */
	boolean isLessThan(final CharacterStruct character);

	/**
	 * Returns the {@literal '<'} comparison of the provided CharacterStructs. If at any point a value does not follow
	 * the expected comparison, the comparison loop with short-circuit.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '<'} operation
	 *
	 * @return the {@literal '<'} comparison of the provided CharacterStructs
	 */
	static BooleanStruct isLessThan(final CharacterStruct... characters) {
		if (characters.length == 0) {
			throw new ErrorException("At least one character required to test equality.");
		}

		CharacterStruct previousCharacter = characters[0];

		boolean result = true;
		for (int i = 1; i < characters.length; i++) {
			final CharacterStruct currentCharacter = characters[i];
			result = previousCharacter.isLessThan(currentCharacter);
			if (!result) {
				break;
			}
			previousCharacter = currentCharacter;
		}
		return BooleanStruct.toLispBoolean(result);
	}

	/**
	 * Returns the {@literal '>'} comparison of this CharacterStruct and the provided CharacterStruct.
	 *
	 * @param character
	 * 		the CharacterStruct to be used in the {@literal '>'} operation
	 *
	 * @return the {@literal '>'} comparison of this CharacterStruct and the provided CharacterStruct
	 */
	boolean isGreaterThan(final CharacterStruct character);

	/**
	 * Returns the {@literal '>'} comparison of the provided CharacterStructs. If at any point a value does not follow
	 * the expected comparison, the comparison loop with short-circuit.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '>'} operation
	 *
	 * @return the {@literal '>'} comparison of the provided CharacterStructs
	 */
	static BooleanStruct isGreaterThan(final CharacterStruct... characters) {
		if (characters.length == 0) {
			throw new ErrorException("At least one character required to test equality.");
		}

		CharacterStruct previousCharacter = characters[0];

		boolean result = true;
		for (int i = 1; i < characters.length; i++) {
			final CharacterStruct currentCharacter = characters[i];
			result = previousCharacter.isGreaterThan(currentCharacter);
			if (!result) {
				break;
			}
			previousCharacter = currentCharacter;
		}
		return BooleanStruct.toLispBoolean(result);
	}

	/**
	 * Returns the {@literal '<='} comparison of this CharacterStruct and the provided CharacterStruct.
	 *
	 * @param character
	 * 		the CharacterStruct to be used in the {@literal '<='} operation
	 *
	 * @return the {@literal '<='} comparison of this CharacterStruct and the provided CharacterStruct
	 */
	boolean isLessThanOrEqualTo(final CharacterStruct character);

	/**
	 * Returns the {@literal '<='} comparison of the provided CharacterStructs. If at any point a value does not follow
	 * the expected comparison, the comparison loop with short-circuit.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '<='} operation
	 *
	 * @return the {@literal '<='} comparison of the provided CharacterStructs
	 */
	static BooleanStruct isLessThanOrEqualTo(final CharacterStruct... characters) {
		if (characters.length == 0) {
			throw new ErrorException("At least one character required to test equality.");
		}

		CharacterStruct previousCharacter = characters[0];

		boolean result = true;
		for (int i = 1; i < characters.length; i++) {
			final CharacterStruct currentCharacter = characters[i];
			result = previousCharacter.isLessThanOrEqualTo(currentCharacter);
			if (!result) {
				break;
			}
			previousCharacter = currentCharacter;
		}
		return BooleanStruct.toLispBoolean(result);
	}

	/**
	 * Returns the {@literal '>='} comparison of this CharacterStruct and the provided CharacterStruct.
	 *
	 * @param character
	 * 		the CharacterStruct to be used in the {@literal '>='} operation
	 *
	 * @return the {@literal '>='} comparison of this CharacterStruct and the provided CharacterStruct
	 */
	boolean isGreaterThanOrEqualTo(final CharacterStruct character);

	/**
	 * Returns the {@literal '>='} comparison of the provided CharacterStructs. If at any point a value does not follow
	 * the expected comparison, the comparison loop with short-circuit.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '>='} operation
	 *
	 * @return the {@literal '>='} comparison of the provided CharacterStructs
	 */
	static BooleanStruct isGreaterThanOrEqualTo(final CharacterStruct... characters) {
		if (characters.length == 0) {
			throw new ErrorException("At least one character required to test equality.");
		}

		CharacterStruct previousCharacter = characters[0];

		boolean result = true;
		for (int i = 1; i < characters.length; i++) {
			final CharacterStruct currentCharacter = characters[i];
			result = previousCharacter.isGreaterThanOrEqualTo(currentCharacter);
			if (!result) {
				break;
			}
			previousCharacter = currentCharacter;
		}
		return BooleanStruct.toLispBoolean(result);
	}

	/**
	 * Returns the '=' comparison of this CharacterStruct and the provided CharacterStruct.
	 *
	 * @param character
	 * 		the CharacterStruct to be used in the '=' operation
	 *
	 * @return the '=' comparison of this CharacterStruct and the provided CharacterStruct
	 */
	boolean isEqualToIgnoreCase(final CharacterStruct character);

	/**
	 * Returns the '=' comparison of the provided CharacterStructs. If at any point a value does not follow
	 * the expected comparison, the comparison loop with short-circuit.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the '=' operation
	 *
	 * @return the '=' comparison of the provided CharacterStructs
	 */
	static BooleanStruct isEqualToIgnoreCase(final CharacterStruct... characters) {
		if (characters.length == 0) {
			throw new ErrorException("At least one character required to test equality.");
		}

		CharacterStruct previousCharacter = characters[0];

		boolean result = true;
		for (int i = 1; i < characters.length; i++) {
			final CharacterStruct currentCharacter = characters[i];
			result = previousCharacter.isEqualToIgnoreCase(currentCharacter);
			if (!result) {
				break;
			}
			previousCharacter = currentCharacter;
		}
		return BooleanStruct.toLispBoolean(result);
	}

	/**
	 * Returns the '!=' comparison of this CharacterStruct and the provided CharacterStruct.
	 *
	 * @param character
	 * 		the CharacterStruct to be used in the '!=' operation
	 *
	 * @return the '!=' comparison of this CharacterStruct and the provided CharacterStruct
	 */
	boolean isNotEqualToIgnoreCase(final CharacterStruct character);

	/**
	 * Returns the '!=' comparison of the provided CharacterStructs. If at any point a value does not follow
	 * the expected comparison, the comparison loop with short-circuit.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the '!=' operation
	 *
	 * @return the '!=' comparison of the provided CharacterStructs
	 */
	static BooleanStruct isNotEqualToIgnoreCase(final CharacterStruct... characters) {
		if (characters.length == 0) {
			throw new ErrorException("At least one character required to test equality.");
		}

		CharacterStruct previousCharacter = characters[0];

		boolean result = true;
		for (int i = 1; i < characters.length; i++) {
			final CharacterStruct currentCharacter = characters[i];
			result = previousCharacter.isNotEqualToIgnoreCase(currentCharacter);
			if (!result) {
				break;
			}
			previousCharacter = currentCharacter;
		}
		return BooleanStruct.toLispBoolean(result);
	}

	/**
	 * Returns the {@literal '<'} comparison of this CharacterStruct and the provided CharacterStruct.
	 *
	 * @param character
	 * 		the CharacterStruct to be used in the {@literal '<'} operation
	 *
	 * @return the {@literal '<'} comparison of this CharacterStruct and the provided CharacterStruct
	 */
	boolean isLessThanIgnoreCase(final CharacterStruct character);

	/**
	 * Returns the {@literal '<'} comparison of the provided CharacterStructs. If at any point a value does not follow
	 * the expected comparison, the comparison loop with short-circuit.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '<'} operation
	 *
	 * @return the {@literal '<'} comparison of the provided CharacterStructs
	 */
	static BooleanStruct isLessThanIgnoreCase(final CharacterStruct... characters) {
		if (characters.length == 0) {
			throw new ErrorException("At least one character required to test equality.");
		}

		CharacterStruct previousCharacter = characters[0];

		boolean result = true;
		for (int i = 1; i < characters.length; i++) {
			final CharacterStruct currentCharacter = characters[i];
			result = previousCharacter.isLessThanIgnoreCase(currentCharacter);
			if (!result) {
				break;
			}
			previousCharacter = currentCharacter;
		}
		return BooleanStruct.toLispBoolean(result);
	}

	/**
	 * Returns the {@literal '>'} comparison of this CharacterStruct and the provided CharacterStruct.
	 *
	 * @param character
	 * 		the CharacterStruct to be used in the {@literal '>'} operation
	 *
	 * @return the {@literal '>'} comparison of this CharacterStruct and the provided CharacterStruct
	 */
	boolean isGreaterThanIgnoreCase(final CharacterStruct character);

	/**
	 * Returns the {@literal '>'} comparison of the provided CharacterStructs. If at any point a value does not follow
	 * the expected comparison, the comparison loop with short-circuit.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '>'} operation
	 *
	 * @return the {@literal '>'} comparison of the provided CharacterStructs
	 */
	static BooleanStruct isGreaterThanIgnoreCase(final CharacterStruct... characters) {
		if (characters.length == 0) {
			throw new ErrorException("At least one character required to test equality.");
		}

		CharacterStruct previousCharacter = characters[0];

		boolean result = true;
		for (int i = 1; i < characters.length; i++) {
			final CharacterStruct currentCharacter = characters[i];
			result = previousCharacter.isGreaterThanIgnoreCase(currentCharacter);
			if (!result) {
				break;
			}
			previousCharacter = currentCharacter;
		}
		return BooleanStruct.toLispBoolean(result);
	}

	/**
	 * Returns the {@literal '<='} comparison of this CharacterStruct and the provided CharacterStruct.
	 *
	 * @param character
	 * 		the CharacterStruct to be used in the {@literal '<='} operation
	 *
	 * @return the {@literal '<='} comparison of this CharacterStruct and the provided CharacterStruct
	 */
	boolean isLessThanOrEqualToIgnoreCase(final CharacterStruct character);

	/**
	 * Returns the {@literal '<='} comparison of the provided CharacterStructs. If at any point a value does not follow
	 * the expected comparison, the comparison loop with short-circuit.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '<='} operation
	 *
	 * @return the {@literal '<='} comparison of the provided CharacterStructs
	 */
	static BooleanStruct isLessThanOrEqualToIgnoreCase(final CharacterStruct... characters) {
		if (characters.length == 0) {
			throw new ErrorException("At least one character required to test equality.");
		}

		CharacterStruct previousCharacter = characters[0];

		boolean result = true;
		for (int i = 1; i < characters.length; i++) {
			final CharacterStruct currentCharacter = characters[i];
			result = previousCharacter.isLessThanOrEqualToIgnoreCase(currentCharacter);
			if (!result) {
				break;
			}
			previousCharacter = currentCharacter;
		}
		return BooleanStruct.toLispBoolean(result);
	}

	/**
	 * Returns the {@literal '>='} comparison of this CharacterStruct and the provided CharacterStruct.
	 *
	 * @param character
	 * 		the CharacterStruct to be used in the {@literal '>='} operation
	 *
	 * @return the {@literal '>='} comparison of this CharacterStruct and the provided CharacterStruct
	 */
	boolean isGreaterThanOrEqualToIgnoreCase(final CharacterStruct character);

	/**
	 * Returns the {@literal '>='} comparison of the provided CharacterStructs. If at any point a value does not follow
	 * the expected comparison, the comparison loop with short-circuit.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '>='} operation
	 *
	 * @return the {@literal '>='} comparison of the provided CharacterStructs
	 */
	static BooleanStruct isGreaterThanOrEqualToIgnoreCase(final CharacterStruct... characters) {
		if (characters.length == 0) {
			throw new ErrorException("At least one character required to test equality.");
		}

		CharacterStruct previousCharacter = characters[0];

		boolean result = true;
		for (int i = 1; i < characters.length; i++) {
			final CharacterStruct currentCharacter = characters[i];
			result = previousCharacter.isGreaterThanOrEqualToIgnoreCase(currentCharacter);
			if (!result) {
				break;
			}
			previousCharacter = currentCharacter;
		}
		return BooleanStruct.toLispBoolean(result);
	}

	/**
	 * Determines if this CharacterStruct is an alphabetic character.
	 *
	 * @return {@code true} if the CharacterStruct is an alphabetic character; {@code false} otherwise
	 */
	BooleanStruct isAlphaChar();

	/**
	 * Determines if this CharacterStruct is an alphanumeric character.
	 *
	 * @return {@code true} if the CharacterStruct is an alphanumeric character; {@code false} otherwise
	 */
	BooleanStruct isAlphanumeric();

	/**
	 * Determines if this CharacterStruct is a digit character.
	 *
	 * @return {@code true} if the CharacterStruct is a digit character; {@code false} otherwise
	 */
	BooleanStruct isDigitChar();

	/**
	 * Determines if this CharacterStruct is a graphic character.
	 *
	 * @return {@code true} if the CharacterStruct is a graphic character; {@code false} otherwise
	 */
	BooleanStruct isGraphicChar();

	/**
	 * Determines if this CharacterStruct is a standard character, meaning a graphic character or the newline
	 * character.
	 *
	 * @return {@code true} if the CharacterStruct is a standard character character; {@code false} otherwise
	 */
	BooleanStruct isStandardChar();

	/**
	 * Converts this CharacterStruct into its equivalent uppercase CharacterStruct.
	 *
	 * @return the equivalent uppercase CharacterStruct
	 */
	CharacterStruct charUpcase();

	/**
	 * Converts this CharacterStruct into its equivalent lowercase CharacterStruct.
	 *
	 * @return the equivalent lowercase CharacterStruct
	 */
	CharacterStruct charDowncase();

	/**
	 * Determines if this CharacterStruct is an uppercase character.
	 *
	 * @return {@code true} if the CharacterStruct is an uppercase character; {@code false} otherwise
	 */
	BooleanStruct isUpperCase();

	/**
	 * Determines if this CharacterStruct is a lowercase character.
	 *
	 * @return {@code true} if the CharacterStruct is a lowercase character; {@code false} otherwise
	 */
	BooleanStruct isLowerCase();

	/**
	 * Determines if this CharacterStruct is an uppercase or lowercase character.
	 *
	 * @return {@code true} if the CharacterStruct is an uppercase or lowercase character; {@code false} otherwise
	 */
	BooleanStruct isBothCase();

	/**
	 * Returns the codePoint of this CharacterStruct as an {@link IntegerStruct}.
	 *
	 * @return the codePoint of this CharacterStruct as an {@link IntegerStruct}
	 */
	IntegerStruct charCode();

	/**
	 * Returns a character with the code attribute given by the provided {@code code}. If not such character exists and
	 * one cannot be created, {@link NILStruct#INSTANCE} is returned.
	 *
	 * @param code
	 * 		the code attribute to convert to a character
	 *
	 * @return a character if is representable by {@code code}, or {@link NILStruct#INSTANCE}
	 */
	static LispStruct codeChar(final IntegerStruct code) {
		final int codeValue = code.toJavaInt();
		if (!Character.isDefined(codeValue)) {
			return NILStruct.INSTANCE;
		}
		return toLispCharacter(codeValue);
	}

	/**
	 * Returns the {@link StringStruct} representing the name of this CharacterStruct.
	 *
	 * @return the {@link StringStruct} representing the name of this CharacterStruct
	 */
	StringStruct charName();

	/**
	 * Returns the character encoding of the CharacterStruct. This defaults to {@link #charCode()}, as the
	 * implementation outcome would be the same.
	 *
	 * @return the character encoding of the CharacterStruct
	 */
	IntegerStruct charInt();

	/**
	 * Determines the digit representation of this character within a defaulted radix of {@code 10}. This is used for
	 * the 'digit-char-p' LISP function.
	 *
	 * @return the character representation of this character within the {@code 10} radix
	 */
	default LispStruct charDigit() {
		return charDigit(IntegerStruct.TEN);
	}

	/**
	 * Determines the digit representation of this character in the specified radix. If the value of the {@code radix}
	 * is not a valid radix via the call to {@link Character#digit(int, int)}, {@link NILStruct#INSTANCE} is returned.
	 * This is used for the 'digit-char-p' LISP function.
	 *
	 * @param radix
	 * 		the radix
	 *
	 * @return the character representation of this character in the specified radix
	 */
	LispStruct charDigit(final IntegerStruct radix);

	/**
	 * Returns a CharacterStruct that has the provided {@code weight} within a defaulted radix of {@code 10}. If weight
	 * is greater than the radix, NIL is returned.
	 *
	 * @param weight
	 * 		the weight of the CharacterStuct to return
	 *
	 * @return a CharacterStruct that has the provided {@code weight}, or NIL
	 */
	static LispStruct digitChar(final IntegerStruct weight) {
		return digitChar(weight, IntegerStruct.TEN);
	}

	/**
	 * Returns a CharacterStruct that has the provided {@code weight} within the provided {@code radix}. If weight is
	 * greater than the radix, NIL is returned.
	 *
	 * @param weight
	 * 		the weight of the CharacterStruct to return
	 * @param radix
	 * 		the radix in which to consider the weight
	 *
	 * @return a CharacterStruct that has the provided {@code weight}, or NIL
	 */
	static LispStruct digitChar(final IntegerStruct weight, final IntegerStruct radix) {
		final int weightInt = weight.toJavaInt();
		final int radixInt = radix.toJavaInt();

		final char digit = Character.forDigit(weightInt, radixInt);
		if (digit == '\0') {
			return NILStruct.INSTANCE;
		}

		final char upperCaseDigit = Character.toUpperCase(digit);
		return toLispCharacter(upperCaseDigit);
	}

	/**
	 * Returns the CharacterStruct whose name is the provided {@code name}. If no such character exists, {@link
	 * NILStruct#INSTANCE} is returned.
	 *
	 * @param name
	 * 		the name of the CharacterStruct to return
	 *
	 * @return the CharacterStruct whose name is the provided {@code name}, or {@link NILStruct#INSTANCE} if no such
	 * character exists
	 *
	 * @throws TypeErrorException
	 * 		if the provided {@code name} is not a valid character-designator (aka, a CharacterStruct, SymbolStruct, or
	 * 		StringStruct)
	 */
	static LispStruct nameChar(final LispStruct name) {
		// TODO: when we have private methods in interfaces in Java 9, let's clean this up a bit.
		if (name instanceof CharacterStruct) {
			return name;
		}
		if (name instanceof SymbolStruct) {
			final SymbolStruct symbol = (SymbolStruct) name;
			final String symbolName = symbol.getName();
			if (CharacterConstants.CL_GRAPHIC_CHAR_NAME_MAP.containsKey(symbolName)) {
				return CharacterConstants.CL_GRAPHIC_CHAR_NAME_MAP.get(symbolName);
			}

			final int unicodeCodePoint = UCharacter.getCharFromName(symbolName);
			if (unicodeCodePoint == -1) {
				return NILStruct.INSTANCE;
			}
			return toLispCharacter(unicodeCodePoint);
		}
		if (name instanceof StringStruct) {
			final StringStruct string = (StringStruct) name;
			final String javaString = string.toJavaString();
			if (CharacterConstants.CL_GRAPHIC_CHAR_NAME_MAP.containsKey(javaString)) {
				return CharacterConstants.CL_GRAPHIC_CHAR_NAME_MAP.get(javaString);
			}

			final int unicodeCodePoint = UCharacter.getCharFromName(javaString);
			if (unicodeCodePoint == -1) {
				return NILStruct.INSTANCE;
			}
			return toLispCharacter(unicodeCodePoint);
		}
		throw new TypeErrorException("Type cannot be converted to CHARACTER.");
	}

	/**
	 * Returns the {@literal char} representation of the CharacterStruct.
	 *
	 * @return a {@literal char} representation of the CharacterStruct
	 */
	char toJavaChar();

	/**
	 * Returns the {@link Character} representation of the CharacterStruct.
	 *
	 * @return a {@link Character} representation of the CharacterStruct
	 */
	Character toJavaCharacter();

	/**
	 * Returns the Unicode {@code codePoint} value of the CharacterStruct.
	 *
	 * @return the Unicode {@code codePoint} value of the CharacterStruct
	 */
	int toUnicodeCodePoint();

	/**
	 * Returns a new CharacterStruct representation of the provided {@literal char}.
	 *
	 * @param character
	 * 		the {@literal char} to represent as a CharacterStruct
	 *
	 * @return a new CharacterStruct representation of the provided {@literal char}
	 */
	static CharacterStruct toLispCharacter(final char character) {
		return toLispCharacter((int) character);
	}

	/**
	 * Returns a new CharacterStruct representation of the provided {@link Character}.
	 *
	 * @param character
	 * 		the {@link Character} to represent as a CharacterStruct
	 *
	 * @return a new CharacterStruct representation of the provided {@link Character}
	 */
	static CharacterStruct toLispCharacter(final Character character) {
		return toLispCharacter(character.charValue());
	}

	/**
	 * Returns a new CharacterStruct representation of the provided Unicode {@code codePoint}.
	 *
	 * @param codePoint
	 * 		a Unicode code-point representing a character
	 *
	 * @return a new CharacterStruct representation of the provided Unicode {@code codePoint}
	 */
	static CharacterStruct toLispCharacter(final int codePoint) {
		final Map<Integer, CharacterStruct> standardCharMap = CharacterConstants.STANDARD_CHAR_MAP;
		if (standardCharMap == null) {
			// This will occur on the initial load only.
			return new CharacterStructImpl(codePoint);
		}

		final CharacterStruct possibleStandardChar = CharacterConstants.STANDARD_CHAR_MAP.get(codePoint);
		if (possibleStandardChar != null) {
			return possibleStandardChar;
		}
		return new CharacterStructImpl(codePoint);
	}

	/**
	 * Returns the provided {@link LispStruct} as a CharacterStruct.
	 *
	 * @param struct
	 * 		the structure to represent as a CharacterStruct
	 *
	 * @return the provided {@link LispStruct} as a CharacterStruct
	 *
	 * @throws SimpleErrorException
	 * 		if a {@link SymbolStruct} or {@link StringStruct} is provided, and is not representative of a single length
	 * 		character
	 * @throws TypeErrorException
	 * 		if the provided {@code name} is not a valid character-designator (aka, a CharacterStruct, SymbolStruct, or
	 * 		StringStruct)
	 */
	static CharacterStruct toLispCharacter(final LispStruct struct) {
		if (struct instanceof CharacterStruct) {
			return (CharacterStruct) struct;
		}
		if (struct instanceof SymbolStruct) {
			final SymbolStruct symbol = (SymbolStruct) struct;
			final String name = symbol.getName();
			if (name.length() != 1) {
				throw new SimpleErrorException("Symbol name is not of length one: " + name);
			}
			return toLispCharacter(name.charAt(0));
		}
		if (struct instanceof StringStruct) {
			final StringStruct string = (StringStruct) struct;
			final String javaString = string.toJavaString();
			if (javaString.length() != 1) {
				throw new SimpleErrorException("String is not of length one: " + javaString);
			}
			return toLispCharacter(javaString.charAt(0));
		}
		throw new TypeErrorException("Type cannot be converted to CHARACTER.");
	}

	/*
	LISP-STRUCT
	 */

	@Override
	default boolean eql(final LispStruct object) {
		return eq(object) ||
				((object instanceof CharacterStruct)
						&& isEqualTo((CharacterStruct) object));
	}

	@Override
	default boolean equalp(final LispStruct object) {
		return equal(object) ||
				((object instanceof CharacterStruct)
						&& isEqualToIgnoreCase((CharacterStruct) object));
	}
}
