/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.character;

import java.math.BigInteger;
import java.util.Map;
import java.util.function.Supplier;

import com.ibm.icu.lang.UCharacter;
import jcl.lang.BuiltInClassStruct;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PrinterVariables;
import jcl.lang.array.StringStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.list.NILStruct;
import jcl.lang.number.IntegerStruct;
import jcl.type.BaseCharType;
import jcl.type.CharacterType;
import jcl.type.ExtendedCharType;
import jcl.type.StandardCharType;
import org.apache.commons.lang3.CharUtils;

/**
 * The {@link CharacterStruct} is the object representation of a Lisp 'character' type.
 */
public class CharacterStruct extends BuiltInClassStruct {

	/**
	 * The code point of the character.
	 */
	private final Integer codePoint;

	/**
	 * Protected constructor.
	 *
	 * @param codePoint
	 * 		the character {@link #codePoint} value
	 */
	protected CharacterStruct(final int codePoint) {
		super(getCharacterType(codePoint), null, null);
		this.codePoint = codePoint;
	}

	/**
	 * This method gets the character type from the provide {@code characterCodePoint}.
	 *
	 * @param characterCodePoint
	 * 		the character {@link #codePoint} value
	 *
	 * @return the matching character type for the provided {@code characterCodePoint}
	 */
	private static CharacterType getCharacterType(final int characterCodePoint) {
		final CharacterType characterType;

		if (CharUtils.isAsciiControl((char) characterCodePoint) && (characterCodePoint != CharUtils.LF)) {
			characterType = BaseCharType.INSTANCE;
		} else if (CharUtils.isAscii((char) characterCodePoint)) {
			characterType = StandardCharType.INSTANCE;
		} else if (Character.isDefined(characterCodePoint)) {
			characterType = ExtendedCharType.INSTANCE;
		} else {
			characterType = CharacterType.INSTANCE;
		}

		return characterType;
	}

	/**
	 * Returns a CharacterStruct object with the provided {@code character} value.
	 *
	 * @param character
	 * 		the character value used to derive the {@link #codePoint} of the resulting CharacterStruct
	 *
	 * @return a CharacterStruct object with the provided {@code character} value
	 */
	public static CharacterStruct valueOf(final Character character) {
		return valueOf((int) character);
	}

	/**
	 * Returns a CharacterStruct object with the provided {@code codePoint} value.
	 *
	 * @param codePoint
	 * 		the {@link #codePoint} value of the resulting CharacterStruct
	 *
	 * @return a CharacterStruct object with the provided {@code codePoint} value
	 */
	public static CharacterStruct valueOf(final Integer codePoint) {
		final Map<Integer, CharacterStruct> standardCharMap = CharacterConstants.STANDARD_CHAR_MAP;
		if (standardCharMap == null) {
			// This will occur on the initial load only.
			return new CharacterStruct(codePoint);
		}

		final CharacterStruct possibleStandardChar = CharacterConstants.STANDARD_CHAR_MAP.get(codePoint);
		if (possibleStandardChar != null) {
			return possibleStandardChar;
		}
		return new CharacterStruct(codePoint);
	}

	/**
	 * Getter for character {@link #codePoint} property.
	 *
	 * @return character {@link #codePoint} property
	 */
	public int getCodePoint() {
		return codePoint;
	}

	/**
	 * Returns the character {@link #codePoint} property as a {@link Character}.
	 *
	 * @return the character {@link #codePoint} property as a {@link Character}
	 */
	public Character getCharacter() {
		return (char) codePoint.intValue();
	}

	/**
	 * {@inheritDoc}
	 * Returns the CharacterStruct instance as is, since it is already a character.
	 *
	 * @return the instance, as it is already a character
	 */
	@Override
	public Supplier<CharacterStruct> asCharacter() {
		return () -> this;
	}

	/**
	 * {@inheritDoc}
	 * Returns the CharacterStruct instance as is, since it is already a character.
	 *
	 * @return the instance, as it is already a character
	 */
	@Override
	public Supplier<CharacterStruct> asNamedCharacter() {
		return () -> this;
	}

	/**
	 * {@inheritDoc}
	 * Returns the PackageStruct with the {@link PackageStruct#name} that matches the CharacterStruct instance via
	 * {@link PackageStruct#findPackage(String)}.
	 *
	 * @return the PackageStruct with the {@link PackageStruct#name} that matches the instance
	 */
	@Override
	public Supplier<PackageStruct> asPackage() {
		return () -> {
			final String packageName = getCharacter().toString();
			return PackageStruct.findPackage(packageName);
		};
	}

	@Override
	public Supplier<StringStruct> asString() {
		return () -> StringStruct.valueOf(getCharacter().toString());
	}

	/**
	 * Returns the '=' comparison of this CharacterStruct and the provided CharacterStruct.
	 *
	 * @param character
	 * 		the CharacterStruct to be used in the '=' operation
	 *
	 * @return the '=' comparison of this CharacterStruct and the provided CharacterStruct
	 */
	private boolean isEqualTo(final CharacterStruct character) {
		return codePoint.compareTo(character.codePoint) == 0;
	}

	/**
	 * Returns the '=' comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the '=' operation
	 *
	 * @return the '=' comparison of the provided IntegerStructs
	 */
	public static boolean isEqualTo(final CharacterStruct... characters) {
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
	private boolean isNotEqualTo(final CharacterStruct character) {
		return codePoint.compareTo(character.codePoint) != 0;
	}

	/**
	 * Returns the '!=' comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the '!=' operation
	 *
	 * @return the '!=' comparison of the provided IntegerStructs
	 */
	public static boolean isNotEqualTo(final CharacterStruct... characters) {
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
	private boolean isLessThan(final CharacterStruct character) {
		return codePoint.compareTo(character.codePoint) < 0;
	}

	/**
	 * Returns the {@literal '<'} comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '<'} operation
	 *
	 * @return the {@literal '<'} comparison of the provided IntegerStructs
	 */
	public static boolean isLessThan(final CharacterStruct... characters) {
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
	private boolean isGreaterThan(final CharacterStruct character) {
		return codePoint.compareTo(character.codePoint) > 0;
	}

	/**
	 * Returns the {@literal '>'} comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '>'} operation
	 *
	 * @return the {@literal '>'} comparison of the provided IntegerStructs
	 */
	public static boolean isGreaterThan(final CharacterStruct... characters) {
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
	private boolean isLessThanOrEqualTo(final CharacterStruct character) {
		return codePoint.compareTo(character.codePoint) <= 0;
	}

	/**
	 * Returns the {@literal '<='} comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '<='} operation
	 *
	 * @return the {@literal '<='} comparison of the provided IntegerStructs
	 */
	public static boolean isLessThanOrEqualTo(final CharacterStruct... characters) {
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
	private boolean isGreaterThanOrEqualTo(final CharacterStruct character) {
		return codePoint.compareTo(character.codePoint) >= 0;
	}

	/**
	 * Returns the {@literal '>='} comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '>='} operation
	 *
	 * @return the {@literal '>='} comparison of the provided IntegerStructs
	 */
	public static boolean isGreaterThanOrEqualTo(final CharacterStruct... characters) {
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
	private boolean isEqualToIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) == Character.toLowerCase(character.codePoint);
	}

	/**
	 * Returns the '=' comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the '=' operation
	 *
	 * @return the '=' comparison of the provided IntegerStructs
	 */
	public static boolean isEqualToIgnoreCase(final CharacterStruct... characters) {
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
	private boolean isNotEqualToIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) != Character.toLowerCase(character.codePoint);
	}

	/**
	 * Returns the '!=' comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the '!=' operation
	 *
	 * @return the '!=' comparison of the provided IntegerStructs
	 */
	public static boolean isNotEqualToIgnoreCase(final CharacterStruct... characters) {
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
	private boolean isLessThanIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) < Character.toLowerCase(character.codePoint);
	}

	/**
	 * Returns the {@literal '<'} comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '<'} operation
	 *
	 * @return the {@literal '<'} comparison of the provided IntegerStructs
	 */
	public static boolean isLessThanIgnoreCase(final CharacterStruct... characters) {
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
	private boolean isGreaterThanIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) > Character.toLowerCase(character.codePoint);
	}

	/**
	 * Returns the {@literal '>'} comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '>'} operation
	 *
	 * @return the {@literal '>'} comparison of the provided IntegerStructs
	 */
	public static boolean isGreaterThanIgnoreCase(final CharacterStruct... characters) {
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
	private boolean isLessThanOrEqualToIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) <= Character.toLowerCase(character.codePoint);
	}

	/**
	 * Returns the {@literal '<='} comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '<='} operation
	 *
	 * @return the {@literal '<='} comparison of the provided IntegerStructs
	 */
	public static boolean isLessThanOrEqualToIgnoreCase(final CharacterStruct... characters) {
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
	private boolean isGreaterThanOrEqualToIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) >= Character.toLowerCase(character.codePoint);
	}

	/**
	 * Returns the {@literal '>='} comparison of the provided CharacterStructs.
	 *
	 * @param characters
	 * 		the CharacterStructs to be used in the {@literal '>='} operation
	 *
	 * @return the {@literal '>='} comparison of the provided IntegerStructs
	 */
	public static boolean isGreaterThanOrEqualToIgnoreCase(final CharacterStruct... characters) {
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
	public boolean isAlphaChar() {
		return Character.isLetter(codePoint);
	}

	/**
	 * Determines if this CharacterStruct is an alphanumeric character.
	 *
	 * @return {@code true} if the CharacterStruct is an alphanumeric character; {@code false} otherwise
	 */
	public boolean isAlphanumeric() {
		return Character.isLetterOrDigit(codePoint);
	}

	/**
	 * Determines if this CharacterStruct is a graphic character.
	 *
	 * @return {@code true} if the CharacterStruct is a graphic character; {@code false} otherwise
	 */
	public boolean isGraphicChar() {
		return CharUtils.isAsciiPrintable(getCharacter());
	}

	/**
	 * Determines if this CharacterStruct is a standard character, meaning a graphic character or the newline
	 * character.
	 *
	 * @return {@code true} if the CharacterStruct is a standard character character; {@code false} otherwise
	 */
	public boolean isStandardChar() {
		return CharUtils.isAsciiPrintable(getCharacter()) || (codePoint == CharUtils.LF);
	}

	/**
	 * Converts this CharacterStruct into its equivalent uppercase CharacterStruct.
	 *
	 * @return the equivalent uppercase CharacterStruct
	 */
	public CharacterStruct toUpperCase() {
		return new CharacterStruct(Character.toUpperCase(codePoint));
	}

	/**
	 * Converts this CharacterStruct into its equivalent lowercase CharacterStruct.
	 *
	 * @return the equivalent lowercase CharacterStruct
	 */
	public CharacterStruct toLowerCase() {
		return new CharacterStruct(Character.toLowerCase(codePoint));
	}

	/**
	 * Determines if this CharacterStruct is an uppercase character.
	 *
	 * @return {@code true} if the CharacterStruct is an uppercase character; {@code false} otherwise
	 */
	public boolean isUpperCase() {
		return Character.isUpperCase(codePoint);
	}

	/**
	 * Determines if this CharacterStruct is a lowercase character.
	 *
	 * @return {@code true} if the CharacterStruct is a lowercase character; {@code false} otherwise
	 */
	public boolean isLowerCase() {
		return Character.isLowerCase(codePoint);
	}

	/**
	 * Determines if this CharacterStruct is an uppercase or lowercase character.
	 *
	 * @return {@code true} if the CharacterStruct is an uppercase or lowercase character; {@code false} otherwise
	 */
	public boolean isBothCase() {
		return Character.isUpperCase(codePoint) || Character.isLowerCase(codePoint);
	}

	/**
	 * Returns the {@link #codePoint} of this CharacterStruct as an {@link IntegerStruct}.
	 *
	 * @return the {@link #codePoint} of this CharacterStruct as an {@link IntegerStruct}
	 */
	public IntegerStruct charCode() {
		return IntegerStruct.valueOf(BigInteger.valueOf(codePoint));
	}

	/**
	 * Returns the CharacterStruct with the code point value of the provided {@link IntegerStruct}. If there is no
	 * defined character via {@link Character#isDefined(int)}, {@link NILStruct#INSTANCE} will be returned instead.
	 *
	 * @param code
	 * 		the code point value of the CharacterStruct to return
	 *
	 * @return the CharacterStruct with the code point value of the provided {@link IntegerStruct} or {@link
	 * NILStruct#INSTANCE} if no character is defined by the code point
	 */
	public static LispStruct codeChar(final IntegerStruct code) {
		final int codePoint = code.intValue();
		if (!Character.isDefined(codePoint)) {
			return NILStruct.INSTANCE;
		}
		return new CharacterStruct(codePoint);
	}

	/**
	 * Returns the {@link StringStruct} representing the name of this CharacterStruct.
	 *
	 * @return the {@link StringStruct} representing the name of this CharacterStruc
	 */
	public StringStruct charName() {
		return StringStruct.valueOf(Character.getName(codePoint));
	}

	/**
	 * Returns the CharacterStruct with the name value of the provided {@link String}.
	 *
	 * @param name
	 * 		the Unicode name value of the CharacterStruct to return
	 *
	 * @return the CharacterStruct with the name value of the provided {@link String}
	 */
	public static CharacterStruct nameChar(final String name) {
		return new CharacterStruct(UCharacter.getCharFromName(name));
	}

	/**
	 * Returns the character encoding of the CharacterStruct. This defaults to {@link #charCode()}, as the
	 * implementation outcome would be the same.
	 *
	 * @return the character encoding of the CharacterStruct
	 */
	public IntegerStruct charInt() {
		return charCode();
	}

	/**
	 * Determines the digit representation of this character in the specified radix. If the value of the {@code radix}
	 * is not a valid radix via the call to {@link Character#digit(int, int)}, {@link NILStruct#INSTANCE} is returned.
	 *
	 * @param radix
	 * 		the radix
	 *
	 * @return the character representation of this character in the specified radix
	 */
	public LispStruct charDigit(final IntegerStruct radix) {
		final int radixInt = radix.intValue();
		final int digit = Character.digit(codePoint, radixInt);
		if (digit == -1) {
			return NILStruct.INSTANCE;
		}
		return IntegerStruct.valueOf(BigInteger.valueOf(digit));
	}

	/**
	 * Determines the character representation for a specific digit (weight) in the specified radix. If the value of
	 * {@code radix} is not a valid radix, or the value of {@code digit} is not a valid digit in the specified radix
	 * via the call to {@link Character#forDigit(int, int)}, {@link NILStruct#INSTANCE} is returned.
	 *
	 * @param weight
	 * 		the number to convert to a character
	 * @param radix
	 * 		the radix
	 *
	 * @return the character representation of the specified digit (weight) in the specified radix
	 */
	public static LispStruct digitChar(final IntegerStruct weight, final IntegerStruct radix) {
		final int weightInt = weight.intValue();
		final int radixInt = radix.intValue();

		final Character digit = Character.forDigit(weightInt, radixInt);
		if (digit == '\0') {
			return NILStruct.INSTANCE;
		}

		final Character result = Character.toUpperCase(digit);
		return new CharacterStruct((int) result);
	}

	@Override
	public String toString() {
		final boolean printEscape = PrinterVariables.PRINT_ESCAPE.getVariableValue().booleanValue();

		final StringBuilder stringBuilder = new StringBuilder();
		if (printEscape) {
			stringBuilder.append("#\\");
		}

		if (Character.isWhitespace(codePoint)) {
			stringBuilder.append(Character.getName(codePoint));
		} else {
			stringBuilder.appendCodePoint(codePoint);
		}

		return stringBuilder.toString();
	}
}
