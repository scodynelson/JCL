package jcl.lang;

import jcl.lang.array.StringStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.list.NILStruct;
import jcl.lang.number.IntegerStruct;

/**
 * The {@link CharacterStruct} is the object representation of a Lisp 'character' type.
 */
public interface CharacterStruct extends LispStruct {

	/**
	 * Getter for character codePoint property.
	 *
	 * @return character codePoint property
	 */
	int getCodePoint();

	/**
	 * Returns the character codePoint property as a {@link Character}.
	 *
	 * @return the character codePoint property as a {@link Character}
	 */
	Character getCharacter();

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
	 * Returns the '=' comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the '=' operation
	 *
	 * @return the '=' comparison of the provided IntegerStructs
	 */
	static boolean isEqualTo(final CharacterStruct... characters) {
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
		return result;
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
	 * Returns the '!=' comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the '!=' operation
	 *
	 * @return the '!=' comparison of the provided IntegerStructs
	 */
	static boolean isNotEqualTo(final CharacterStruct... characters) {
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
		return result;
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
	 * Returns the {@literal '<'} comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '<'} operation
	 *
	 * @return the {@literal '<'} comparison of the provided IntegerStructs
	 */
	static boolean isLessThan(final CharacterStruct... characters) {
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
		return result;
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
	 * Returns the {@literal '>'} comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '>'} operation
	 *
	 * @return the {@literal '>'} comparison of the provided IntegerStructs
	 */
	static boolean isGreaterThan(final CharacterStruct... characters) {
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
		return result;
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
	 * Returns the {@literal '<='} comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '<='} operation
	 *
	 * @return the {@literal '<='} comparison of the provided IntegerStructs
	 */
	static boolean isLessThanOrEqualTo(final CharacterStruct... characters) {
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
		return result;
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
	 * Returns the {@literal '>='} comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '>='} operation
	 *
	 * @return the {@literal '>='} comparison of the provided IntegerStructs
	 */
	static boolean isGreaterThanOrEqualTo(final CharacterStruct... characters) {
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
		return result;
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
	 * Returns the '=' comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the '=' operation
	 *
	 * @return the '=' comparison of the provided IntegerStructs
	 */
	static boolean isEqualToIgnoreCase(final CharacterStruct... characters) {
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
		return result;
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
	 * Returns the '!=' comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the '!=' operation
	 *
	 * @return the '!=' comparison of the provided IntegerStructs
	 */
	static boolean isNotEqualToIgnoreCase(final CharacterStruct... characters) {
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
		return result;
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
	 * Returns the {@literal '<'} comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '<'} operation
	 *
	 * @return the {@literal '<'} comparison of the provided IntegerStructs
	 */
	static boolean isLessThanIgnoreCase(final CharacterStruct... characters) {
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
		return result;
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
	 * Returns the {@literal '>'} comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '>'} operation
	 *
	 * @return the {@literal '>'} comparison of the provided IntegerStructs
	 */
	static boolean isGreaterThanIgnoreCase(final CharacterStruct... characters) {
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
		return result;
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
	 * Returns the {@literal '<='} comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '<='} operation
	 *
	 * @return the {@literal '<='} comparison of the provided IntegerStructs
	 */
	static boolean isLessThanOrEqualToIgnoreCase(final CharacterStruct... characters) {
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
		return result;
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
	 * Returns the {@literal '>='} comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '>='} operation
	 *
	 * @return the {@literal '>='} comparison of the provided IntegerStructs
	 */
	static boolean isGreaterThanOrEqualToIgnoreCase(final CharacterStruct... characters) {
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
		return result;
	}

	/**
	 * Determines if this CharacterStruct is an alphabetic character.
	 *
	 * @return {@code true} if the CharacterStruct is an alphabetic character; {@code false} otherwise
	 */
	boolean isAlphaChar();

	/**
	 * Determines if this CharacterStruct is an alphanumeric character.
	 *
	 * @return {@code true} if the CharacterStruct is an alphanumeric character; {@code false} otherwise
	 */
	boolean isAlphanumeric();

	/**
	 * Determines if this CharacterStruct is a graphic character.
	 *
	 * @return {@code true} if the CharacterStruct is a graphic character; {@code false} otherwise
	 */
	boolean isGraphicChar();

	/**
	 * Determines if this CharacterStruct is a standard character, meaning a graphic character or the newline
	 * character.
	 *
	 * @return {@code true} if the CharacterStruct is a standard character character; {@code false} otherwise
	 */
	boolean isStandardChar();

	/**
	 * Converts this CharacterStruct into its equivalent uppercase CharacterStruct.
	 *
	 * @return the equivalent uppercase CharacterStruct
	 */
	CharacterStruct toUpperCase();

	/**
	 * Converts this CharacterStruct into its equivalent lowercase CharacterStruct.
	 *
	 * @return the equivalent lowercase CharacterStruct
	 */
	CharacterStruct toLowerCase();

	/**
	 * Determines if this CharacterStruct is an uppercase character.
	 *
	 * @return {@code true} if the CharacterStruct is an uppercase character; {@code false} otherwise
	 */
	boolean isUpperCase();

	/**
	 * Determines if this CharacterStruct is a lowercase character.
	 *
	 * @return {@code true} if the CharacterStruct is a lowercase character; {@code false} otherwise
	 */
	boolean isLowerCase();

	/**
	 * Determines if this CharacterStruct is an uppercase or lowercase character.
	 *
	 * @return {@code true} if the CharacterStruct is an uppercase or lowercase character; {@code false} otherwise
	 */
	boolean isBothCase();

	/**
	 * Returns the codePoint of this CharacterStruct as an {@link IntegerStruct}.
	 *
	 * @return the codePoint of this CharacterStruct as an {@link IntegerStruct}
	 */
	IntegerStruct charCode();

	/**
	 * Returns the {@link StringStruct} representing the name of this CharacterStruct.
	 *
	 * @return the {@link StringStruct} representing the name of this CharacterStruc
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
	 * Determines the digit representation of this character in the specified radix. If the value of the {@code radix}
	 * is not a valid radix via the call to {@link Character#digit(int, int)}, {@link NILStruct#INSTANCE} is returned.
	 *
	 * @param radix
	 * 		the radix
	 *
	 * @return the character representation of this character in the specified radix
	 */
	LispStruct charDigit(final IntegerStruct radix);
}
