/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

/**
 * The {@link InputStreamStruct} is the representation for all Lisp input 'stream' types.
 */
public interface InputStreamStruct extends StreamStruct {

	/**
	 * Reads a character from the stream.
	 *
	 * @param eofErrorP
	 * 		whether or not to either throw an error or return gracefully
	 * @param eofValue
	 * 		the graceful return value when an error occurs
	 *
	 * @return the character read from the stream
	 */
	ReadCharResult readChar(final boolean eofErrorP, final LispStruct eofValue);

	/**
	 * Reads a character from the stream.
	 *
	 * @param eofErrorP
	 * 		whether or not to either throw an error or return gracefully
	 * @param eofValue
	 * 		the graceful return value when an error occurs
	 *
	 * @return the character read from the stream
	 */
	default LispStruct readChar(final BooleanStruct eofErrorP, final LispStruct eofValue) {
		final ReadCharResult readCharResult = readChar(eofErrorP.toJavaPBoolean(), eofValue);
		return readCharResult.isEof() ? eofValue : CharacterStruct.toLispCharacter(readCharResult.getResult());
	}

	/**
	 * Reads a character from the stream, returning immediately if no character is available.
	 *
	 * @param eofErrorP
	 * 		whether or not to either throw an error or return gracefully
	 * @param eofValue
	 * 		the graceful return value when an error occurs
	 *
	 * @return the character read from the stream
	 */
	ReadCharResult readCharNoHang(final boolean eofErrorP, final LispStruct eofValue);

	/**
	 * Reads a character from the stream, returning immediately if no character is available.
	 *
	 * @param eofErrorP
	 * 		whether or not to either throw an error or return gracefully
	 * @param eofValue
	 * 		the graceful return value when an error occurs
	 *
	 * @return the character read from the stream
	 */
	default LispStruct readCharNoHang(final BooleanStruct eofErrorP, final LispStruct eofValue) {
		final ReadCharResult readCharResult = readCharNoHang(eofErrorP.toJavaPBoolean(), eofValue);
		final Integer result = readCharResult.getResult();
		if (result == -5) {
			return NILStruct.INSTANCE;
		}
		return readCharResult.isEof() ? eofValue : CharacterStruct.toLispCharacter(result);
	}

	/**
	 * Reads a byte from the stream.
	 *
	 * @param eofErrorP
	 * 		whether or not to either throw an error or return gracefully
	 * @param eofValue
	 * 		the graceful return value when an error occurs
	 *
	 * @return the byte read from the stream
	 */
	ReadCharResult readByte(final boolean eofErrorP, final LispStruct eofValue);

	/**
	 * Reads a byte from the stream.
	 *
	 * @param eofErrorP
	 * 		whether or not to either throw an error or return gracefully
	 * @param eofValue
	 * 		the graceful return value when an error occurs
	 *
	 * @return the byte read from the stream
	 */
	default LispStruct readByte(final BooleanStruct eofErrorP, final LispStruct eofValue) {
		final ReadCharResult readCharResult = readByte(eofErrorP.toJavaPBoolean(), eofValue);
		return readCharResult.isEof() ? eofValue : IntegerStruct.toLispInteger(readCharResult.getResult());
	}

	/**
	 * Un-reads a character from the stream.
	 *
	 * @param codePoint
	 * 		the codePoint value to un-read back into the stream
	 *
	 * @return the codePoint un-read back into the stream
	 */
	Integer unreadChar(final Integer codePoint);

	/**
	 * Un-reads a character from the stream.
	 *
	 * @param character
	 * 		the character value to un-read back into the stream
	 *
	 * @return the character un-read back into the stream
	 */
	default CharacterStruct unreadChar(final CharacterStruct character) {
		unreadChar(character.toUnicodeCodePoint());
		return character;
	}

	/**
	 * Reads a line from the stream.
	 *
	 * @param eofErrorP
	 * 		whether or not to either throw an error or return gracefully
	 * @param eofValue
	 * 		the graceful return value when an error occurs
	 *
	 * @return the line read from the stream
	 */
	default ReadLineResult readLine(final boolean eofErrorP, final LispStruct eofValue) {
		final StringBuilder stringBuilder = new StringBuilder();

		ReadCharResult readCharResult = readChar(eofErrorP, eofValue);
		Integer result = readCharResult.getResult();
		while (!readCharResult.isEof() && (result != '\n')) {
			stringBuilder.appendCodePoint(result);

			readCharResult = readChar(eofErrorP, eofValue);
			result = readCharResult.getResult();
		}

		if (readCharResult.isEof()) {
			if (stringBuilder.isEmpty()) {
				return new ReadLineResult(eofValue, TStruct.INSTANCE);
			}
			final String resultString = stringBuilder.toString();
			return new ReadLineResult(StringStruct.toLispString(resultString), TStruct.INSTANCE);
		} else {
			final String resultString = stringBuilder.toString();
			return new ReadLineResult(StringStruct.toLispString(resultString), NILStruct.INSTANCE);
		}
	}

	/**
	 * Reads a line from the stream.
	 *
	 * @param eofErrorP
	 * 		whether or not to either throw an error or return gracefully
	 * @param eofValue
	 * 		the graceful return value when an error occurs
	 *
	 * @return the line read from the stream
	 */
	default ReadLineResult readLine(final BooleanStruct eofErrorP, final LispStruct eofValue) {
		return readLine(eofErrorP.toJavaPBoolean(), eofValue);
	}

	/**
	 * Clears the input from the stream.
	 *
	 * @return NIL
	 */
	default LispStruct clearInput() {
		return NILStruct.INSTANCE;
	}

	/**
	 * Listens on the stream to determine if there is any data left to read.
	 *
	 * @return whether or not there is data left to read from the stream
	 */
	BooleanStruct listen();

	/*
	STREAM-STRUCT
	 */

	@Override
	default BooleanStruct inputStreamP() {
		return TStruct.INSTANCE;
	}
}
