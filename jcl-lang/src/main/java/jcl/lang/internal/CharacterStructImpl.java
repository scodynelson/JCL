/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal;

import java.math.BigInteger;
import java.util.Map;
import java.util.function.Supplier;

import jcl.lang.BuiltInClassStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.statics.PrinterVariables;
import jcl.lang.StringStruct;
import jcl.lang.statics.CharacterConstants;
import jcl.lang.list.NILStruct;
import jcl.lang.internal.number.IntegerStructImpl;
import jcl.type.BaseCharType;
import jcl.type.CharacterType;
import jcl.type.ExtendedCharType;
import jcl.type.StandardCharType;
import org.apache.commons.lang3.CharUtils;

/**
 * The {@link CharacterStructImpl} is the object representation of a Lisp 'character' type.
 */
public final class CharacterStructImpl extends BuiltInClassStruct implements CharacterStruct {

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
	private CharacterStructImpl(final int codePoint) {
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
			return new CharacterStructImpl(codePoint);
		}

		final CharacterStruct possibleStandardChar = CharacterConstants.STANDARD_CHAR_MAP.get(codePoint);
		if (possibleStandardChar != null) {
			return possibleStandardChar;
		}
		return new CharacterStructImpl(codePoint);
	}

	@Override
	public int getCodePoint() {
		return codePoint;
	}

	@Override
	public Character getCharacter() {
		return (char) codePoint.intValue();
	}

	@Override
	public Supplier<CharacterStruct> asCharacter() {
		return () -> this;
	}

	@Override
	public Supplier<CharacterStruct> asNamedCharacter() {
		return () -> this;
	}

	@Override
	public Supplier<PackageStruct> asPackage() {
		return () -> {
			final String packageName = getCharacter().toString();
			return PackageStruct.findPackage(packageName);
		};
	}

	@Override
	public Supplier<StringStruct> asString() {
		return () -> StringStructImpl.valueOf(getCharacter().toString());
	}

	@Override
	public boolean isEqualTo(final CharacterStruct character) {
		return codePoint.compareTo(character.getCodePoint()) == 0;
	}

	@Override
	public boolean isNotEqualTo(final CharacterStruct character) {
		return codePoint.compareTo(character.getCodePoint()) != 0;
	}

	@Override
	public boolean isLessThan(final CharacterStruct character) {
		return codePoint.compareTo(character.getCodePoint()) < 0;
	}

	@Override
	public boolean isGreaterThan(final CharacterStruct character) {
		return codePoint.compareTo(character.getCodePoint()) > 0;
	}

	@Override
	public boolean isLessThanOrEqualTo(final CharacterStruct character) {
		return codePoint.compareTo(character.getCodePoint()) <= 0;
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final CharacterStruct character) {
		return codePoint.compareTo(character.getCodePoint()) >= 0;
	}

	@Override
	public boolean isEqualToIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) == Character.toLowerCase(character.getCodePoint());
	}

	@Override
	public boolean isNotEqualToIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) != Character.toLowerCase(character.getCodePoint());
	}

	@Override
	public boolean isLessThanIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) < Character.toLowerCase(character.getCodePoint());
	}

	@Override
	public boolean isGreaterThanIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) > Character.toLowerCase(character.getCodePoint());
	}

	@Override
	public boolean isLessThanOrEqualToIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) <= Character.toLowerCase(character.getCodePoint());
	}

	@Override
	public boolean isGreaterThanOrEqualToIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) >= Character.toLowerCase(character.getCodePoint());
	}

	@Override
	public boolean isAlphaChar() {
		return Character.isLetter(codePoint);
	}

	@Override
	public boolean isAlphanumeric() {
		return Character.isLetterOrDigit(codePoint);
	}

	@Override
	public boolean isGraphicChar() {
		return CharUtils.isAsciiPrintable(getCharacter());
	}

	@Override
	public boolean isStandardChar() {
		return CharUtils.isAsciiPrintable(getCharacter()) || (codePoint == CharUtils.LF);
	}

	@Override
	public CharacterStruct toUpperCase() {
		return new CharacterStructImpl(Character.toUpperCase(codePoint));
	}

	@Override
	public CharacterStruct toLowerCase() {
		return new CharacterStructImpl(Character.toLowerCase(codePoint));
	}

	@Override
	public boolean isUpperCase() {
		return Character.isUpperCase(codePoint);
	}

	@Override
	public boolean isLowerCase() {
		return Character.isLowerCase(codePoint);
	}

	@Override
	public boolean isBothCase() {
		return Character.isUpperCase(codePoint) || Character.isLowerCase(codePoint);
	}

	@Override
	public IntegerStruct charCode() {
		return IntegerStructImpl.valueOf(BigInteger.valueOf(codePoint));
	}

	@Override
	public StringStruct charName() {
		return StringStructImpl.valueOf(Character.getName(codePoint));
	}

	@Override
	public IntegerStruct charInt() {
		return charCode();
	}

	@Override
	public LispStruct charDigit(final IntegerStruct radix) {
		final int radixInt = radix.intValue();
		final int digit = Character.digit(codePoint, radixInt);
		if (digit == -1) {
			return NILStruct.INSTANCE;
		}
		return IntegerStructImpl.valueOf(BigInteger.valueOf(digit));
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
