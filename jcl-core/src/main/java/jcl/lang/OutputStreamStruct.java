/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

/**
 * The {@link OutputStreamStruct} is the representation for all Lisp output 'stream' types.
 */
public interface OutputStreamStruct extends StreamStruct {

	/*
	OUTPUT-STREAM-STRUCT
	 */

	/**
	 * Writes the provided {@code codePoint} out to the stream.
	 *
	 * @param codePoint
	 * 		the character to write out to the stream
	 */
	void writeChar(final int codePoint);

	/**
	 * Writes the provided {@code character} out to the stream.
	 *
	 * @param character
	 * 		the character to write out to the stream
	 *
	 * @return the written character
	 */
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
	void writeByte(final int aByte);

	/**
	 * Writes the provided {@code byteVal} out to the stream.
	 *
	 * @param byteVal
	 * 		the byte to write out to the stream
	 *
	 * @return the written byte
	 */
	default IntegerStruct writeByte(final IntegerStruct byteVal) {
		writeByte(byteVal.toJavaInt());
		return byteVal;
	}

	/**
	 * Writes the provided {@code outputString} to the stream.
	 *
	 * @param outputString
	 * 		the string to write out to the stream
	 */
	void writeString(final String outputString);

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
	 *
	 * @return the written {@link StringStruct}
	 */
	default StringStruct writeString(final StringStruct outputString, final FixnumStruct start,
	                                 final FixnumStruct end) {
		final String javaString = outputString.toJavaString().substring(start.toJavaInt(), end.toJavaInt());
		writeString(javaString);
		return outputString;
	}

	/**
	 * Writes the provided {@code outputString} to the stream, followed by a newline character.
	 *
	 * @param outputString
	 * 		the string to write out to the stream
	 */
	void writeLine(final String outputString);

	/**
	 * Writes the provided {@code outputString} from the provided {@code start} index to the provided {@code end} index
	 * to the stream, followed by a newline character.
	 *
	 * @param outputString
	 * 		the string to write out to the stream
	 * @param start
	 * 		the starting index
	 * @param end
	 * 		the ending index
	 *
	 * @return the written {@link StringStruct}
	 */
	default StringStruct writeLine(final StringStruct outputString, final FixnumStruct start,
	                               final FixnumStruct end) {
		final String javaString = outputString.toJavaString().substring(start.toJavaInt(), end.toJavaInt());
		writeLine(javaString);
		return outputString;
	}

	/**
	 * Outputs a newline character if the last character output to the stream is not a newline character.
	 *
	 * @return T if a newline character was written; NIL otherwise
	 */
	BooleanStruct freshLine();

	/**
	 * Outputs a newline character to the stream.
	 *
	 * @return NIL
	 */
	BooleanStruct terpri();

	/**
	 * Clears the output from the stream.
	 *
	 * @return NIL
	 */
	default LispStruct clearOutput() {
		return NILStruct.INSTANCE;
	}

	/**
	 * Finishes the output currently in the stream.
	 *
	 * @return NIL
	 */
	default LispStruct finishOutput() {
		return NILStruct.INSTANCE;
	}

	/**
	 * Forces the output through the stream.
	 *
	 * @return NIL
	 */
	default LispStruct forceOutput() {
		return NILStruct.INSTANCE;
	}

	/*
	STREAM-STRUCT
	 */

	@Override
	default BooleanStruct outputStreamP() {
		return TStruct.INSTANCE;
	}
}
