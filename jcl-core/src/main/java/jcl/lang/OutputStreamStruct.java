/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

/**
 * The {@link OutputStreamStruct} is the representation for all Lisp output 'stream' types.
 */
public interface OutputStreamStruct extends StreamStruct {

	@Override
	default boolean close(final boolean abort) {
		if (abort) {
			clearOutput();
		} else {
			forceOutput();
		}
		return close();
	}

	/**
	 * Writes the provided {@code aChar} out to the stream.
	 *
	 * @param aChar
	 * 		the character to write out to the stream
	 */
	void writeChar(int aChar);

	default CharacterStruct writeChar(final CharacterStruct character) {
		writeChar(character.toUnicodeCodePoint());
		return character;
	}

	/**
	 * Writes the provided {@code aByte} out to the stream.
	 *
	 * @param aByte
	 * 		the byte to write out to the stream
	 */
	void writeByte(int aByte);

	default IntegerStruct writeByte(final IntegerStruct byteVal) {
		final int byteValue = byteVal.toJavaInt();
		writeByte(byteValue);
		return byteVal;
	}

	/**
	 * Writes the provided {@code outputString} from the provided {@code start} index to the provided {@code end} index
	 * to the stream.
	 *
	 * @param outputString
	 * 		the string to write out to the stream
	 * @param start
	 * 		the starting index
	 */
	default void writeString(final String outputString, final int start) {
		writeString(outputString, start, outputString.length() - 1);
	}

	/**
	 * Writes the provided {@code outputString} from the provided {@code start} index to the provided {@code end} index
	 * to the stream.
	 *
	 * @param outputString
	 * 		the string to write out to the stream
	 * @param start
	 * 		the starting index
	 * @param end
	 * 		the ending index
	 */
	default void writeString(final String outputString, final int start, final int end) {
		final String subString = outputString.substring(start, end);
		subString.chars().forEach(this::writeChar);
	}

	default StringStruct writeString(final StringStruct stringParam, final IntegerStruct startParam, final LispStruct endParam) {
		final int start = startParam.toJavaInt();

		final String javaString = stringParam.toJavaString();
		if (endParam instanceof IntegerStruct) {
			final int end = ((IntegerStruct) endParam).toJavaInt();
			writeString(javaString, start, end);
		} else {
			writeString(javaString, start);
		}
		return stringParam;
	}

	default void writeLine(final String outputString, final int start) {
		writeString(outputString, start);
		writeChar('\n');
	}

	default void writeLine(final String outputString, final int start, final int end) {
		writeString(outputString, start, end);
		writeChar('\n');
	}

	default StringStruct writeLine(final StringStruct stringParam, final IntegerStruct startParam, final LispStruct endParam) {
		final int start = startParam.toJavaInt();

		final String javaString = stringParam.toJavaString();
		if (endParam instanceof IntegerStruct) {
			final int end = ((IntegerStruct) endParam).toJavaInt();
			writeLine(javaString, start, end);
		} else {
			writeLine(javaString, start);
		}
		return stringParam;
	}

	/**
	 * Clears the output from the stream.
	 */
	void clearOutput();

	default LispStruct clearOutput1() {
		// TODO: Fix method name
		clearOutput();
		return NILStruct.INSTANCE;
	}

	/**
	 * Finishes the output currently in the stream.
	 */
	void finishOutput();

	default LispStruct finishOutput1() {
		// TODO: Fix method name
		finishOutput();
		return NILStruct.INSTANCE;
	}

	/**
	 * Forces the output through the stream.
	 */
	void forceOutput();

	default LispStruct forceOutput1() {
		// TODO: Fix method name
		forceOutput();
		return NILStruct.INSTANCE;
	}

	@Override
	default boolean isOutputStream() {
		return true;
	}

	boolean isStartOfLine();

	default BooleanStruct freshLine() {
		final boolean shouldWriteNewline = !isStartOfLine();
		if (shouldWriteNewline) {
			writeChar('\n');
		}

		return BooleanStruct.toLispBoolean(shouldWriteNewline);
	}

	default BooleanStruct terpri() {
		writeChar('\n');
		return NILStruct.INSTANCE;
	}
}
