/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.StringStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.statics.PrinterVariables;
import jcl.type.BaseCharType;
import jcl.type.CharacterType;
import jcl.type.ExtendedCharType;
import jcl.type.StandardCharType;
import jcl.util.CodePointConstants;
import lombok.EqualsAndHashCode;
import org.apache.commons.lang3.CharUtils;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

/**
 * The {@link CharacterStructImpl} is the object representation of a Lisp 'character' type.
 */
@EqualsAndHashCode(callSuper = false)
public final class CharacterStructImpl extends BuiltInClassStruct implements CharacterStruct {

	/**
	 * The code point of the character.
	 */
	private final int codePoint;

	/**
	 * Public constructor.
	 *
	 * @param codePoint
	 * 		the character {@link #codePoint} value
	 */
	public CharacterStructImpl(final int codePoint) {
		super(getCharacterType(codePoint), null, null);
		this.codePoint = codePoint;
	}

	/**
	 * This method gets the character type from the provide {@code codePoint}.
	 *
	 * @param codePoint
	 * 		the character value
	 *
	 * @return the matching character type for the provided {@code codePoint}
	 */
	private static CharacterType getCharacterType(final int codePoint) {
		final CharacterType characterType;

		if (CharUtils.isAsciiControl((char) codePoint) && (codePoint != CharUtils.LF)) {
			characterType = BaseCharType.INSTANCE;
		} else if (CharUtils.isAscii((char) codePoint)) {
			characterType = StandardCharType.INSTANCE;
		} else if (Character.isDefined(codePoint)) {
			characterType = ExtendedCharType.INSTANCE;
		} else {
			characterType = CharacterType.INSTANCE;
		}

		return characterType;
	}

	@Override
	public boolean isEqualTo(final CharacterStruct character) {
		return (char) codePoint == character.toJavaChar();
	}

	@Override
	public boolean isNotEqualTo(final CharacterStruct character) {
		return (char) codePoint != character.toJavaChar();
	}

	@Override
	public boolean isLessThan(final CharacterStruct character) {
		return (char) codePoint < character.toJavaChar();
	}

	@Override
	public boolean isGreaterThan(final CharacterStruct character) {
		return (char) codePoint > character.toJavaChar();
	}

	@Override
	public boolean isLessThanOrEqualTo(final CharacterStruct character) {
		return (char) codePoint <= character.toJavaChar();
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final CharacterStruct character) {
		return (char) codePoint >= character.toJavaChar();
	}

	@Override
	public boolean isEqualToIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) == Character.toLowerCase(character.toUnicodeCodePoint());
	}

	@Override
	public boolean isNotEqualToIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) != Character.toLowerCase(character.toUnicodeCodePoint());
	}

	@Override
	public boolean isLessThanIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) < Character.toLowerCase(character.toUnicodeCodePoint());
	}

	@Override
	public boolean isGreaterThanIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) > Character.toLowerCase(character.toUnicodeCodePoint());
	}

	@Override
	public boolean isLessThanOrEqualToIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) <= Character.toLowerCase(character.toUnicodeCodePoint());
	}

	@Override
	public boolean isGreaterThanOrEqualToIgnoreCase(final CharacterStruct character) {
		return Character.toLowerCase(codePoint) >= Character.toLowerCase(character.toUnicodeCodePoint());
	}

	@Override
	public BooleanStruct isAlphaChar() {
		return BooleanStruct.toLispBoolean(Character.isLetter(codePoint));
	}

	@Override
	public BooleanStruct isAlphanumeric() {
		return BooleanStruct.toLispBoolean(Character.isLetterOrDigit(codePoint));
	}

	@Override
	public BooleanStruct isDigitChar() {
		return BooleanStruct.toLispBoolean(Character.isDigit(codePoint));
	}

	@Override
	public BooleanStruct isGraphicChar() {
		return BooleanStruct.toLispBoolean(CharUtils.isAsciiPrintable((char) codePoint));
	}

	@Override
	public BooleanStruct isStandardChar() {
		return BooleanStruct.toLispBoolean(
				CharUtils.isAsciiPrintable((char) codePoint) || (codePoint == CodePointConstants.NEWLINE)
		);
	}

	@Override
	public CharacterStruct charUpcase() {
		return CharacterStruct.toLispCharacter(Character.toUpperCase(codePoint));
	}

	@Override
	public CharacterStruct charDowncase() {
		return CharacterStruct.toLispCharacter(Character.toLowerCase(codePoint));
	}

	@Override
	public BooleanStruct isUpperCase() {
		return BooleanStruct.toLispBoolean(Character.isUpperCase(codePoint));
	}

	@Override
	public BooleanStruct isLowerCase() {
		return BooleanStruct.toLispBoolean(Character.isLowerCase(codePoint));
	}

	@Override
	public BooleanStruct isBothCase() {
		return BooleanStruct.toLispBoolean(Character.isUpperCase(codePoint) || Character.isLowerCase(codePoint));
	}

	@Override
	public IntegerStruct charCode() {
		return IntegerStruct.toLispInteger(codePoint);
	}

	@Override
	public StringStruct charName() {
		return StringStruct.toLispString(Character.getName(codePoint));
	}

	@Override
	public IntegerStruct charInt() {
		return charCode();
	}

	@Override
	public LispStruct charDigit(final IntegerStruct radix) {
		final int radixInt = radix.toJavaInt();
		final int digit = Character.digit(codePoint, radixInt);
		if (digit == -1) {
			return NILStruct.INSTANCE;
		}
		return IntegerStruct.toLispInteger(digit);
	}

	@Override
	public char toJavaChar() {
		return (char) codePoint;
	}

	@Override
	public Character toJavaCharacter() {
		return toJavaChar();
	}

	@Override
	public int toUnicodeCodePoint() {
		return codePoint;
	}

	/*
	LISP-STRUCT
	 */

	/**
	 * {@inheritDoc}
	 * Generation method for {@link CharacterStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Loading the {@link CharacterStruct#toUnicodeCodePoint()} constant</li>
	 * <li>Retrieving a {@link CharacterStruct} with the loaded code point value</li>
	 * </ol>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@Override
	public void generate(final GeneratorState generatorState) {
		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitLdcInsn(codePoint);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.JAVA_INTEGER_NAME,
		                   GenerationConstants.JAVA_INTEGER_VALUE_OF_METHOD_NAME,
		                   GenerationConstants.JAVA_INTEGER_VALUE_OF_METHOD_DESC,
		                   false);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
		                   GenerationConstants.CHARACTER_STRUCT_NAME,
		                   GenerationConstants.CHARACTER_STRUCT_TO_LISP_CHARACTER_METHOD_NAME,
		                   GenerationConstants.CHARACTER_STRUCT_TO_LISP_CHARACTER_METHOD_DESC,
		                   false);
	}

	/*
	OBJECT
	 */

	@Override
	public String toString() {
		final boolean printEscape = PrinterVariables.PRINT_ESCAPE.getVariableValue().toJavaPBoolean();

		final StringBuilder stringBuilder = new StringBuilder();
		if (printEscape) {
			stringBuilder.append("#\\");
		}
		stringBuilder.appendCodePoint(codePoint);

		return stringBuilder.toString();
	}
}
