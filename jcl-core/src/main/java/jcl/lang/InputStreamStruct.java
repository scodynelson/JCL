/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.stream.PeekType;
import jcl.lang.stream.ReadLineResult;
import jcl.lang.stream.ReadPeekResult;

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
	 * @param recursiveP
	 * 		whether or not to recursively read and process the character
	 *
	 * @return the character read from the stream
	 */
	ReadPeekResult readChar(boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	default LispStruct readChar(final BooleanStruct eofErrorP,
	                            final LispStruct eofValue,
	                            final BooleanStruct recursiveP) {

		final ReadPeekResult readPeekResult = readChar(eofErrorP.toJavaPBoolean(), eofValue,
		                                               recursiveP.toJavaPBoolean());
		return readPeekResult.isEof() ? eofValue : CharacterStruct.toLispCharacter(readPeekResult.getResult());
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
	ReadPeekResult readByte(boolean eofErrorP, LispStruct eofValue);

	default LispStruct readByte(final BooleanStruct eofErrorP, final LispStruct eofValue) {
		final ReadPeekResult readPeekResult = readByte(eofErrorP.toJavaPBoolean(), eofValue);
		return readPeekResult.isEof() ? eofValue : IntegerStruct.toLispInteger(readPeekResult.getResult());
	}

	/**
	 * Peeks at the next available character in the stream.
	 *
	 * @param peekType
	 * 		how to peek and find the next character in the stream
	 * @param eofErrorP
	 * 		whether or not to either throw an error or return gracefully
	 * @param eofValue
	 * 		the graceful return value when an error occurs
	 * @param recursiveP
	 * 		whether or not to recursively read and process the character
	 *
	 * @return the next character available in the stream
	 */
	ReadPeekResult peekChar(PeekType peekType, boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	default LispStruct peekChar(final SymbolStruct peekTypeSymbol,
	                            final BooleanStruct eofErrorP,
	                            final LispStruct eofValue,
	                            final BooleanStruct recursiveP) {

		final PeekType peekType;
		if (TStruct.INSTANCE.eq(peekTypeSymbol)) {
			peekType = PeekType.T_PEEK_TYPE;
		} else if (NILStruct.INSTANCE.eq(peekTypeSymbol)) {
			peekType = PeekType.NIL_PEEK_TYPE;
		} else if (peekTypeSymbol instanceof CharacterStruct) {
			final CharacterStruct character = (CharacterStruct) peekTypeSymbol;
			peekType = PeekType.getCharacterPeekType(character.toUnicodeCodePoint());
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}

		final ReadPeekResult readPeekResult = peekChar(peekType, eofErrorP.toJavaPBoolean(), eofValue,
		                                               recursiveP.toJavaPBoolean());
		return readPeekResult.isEof() ? eofValue : CharacterStruct.toLispCharacter(readPeekResult.getResult());
	}

	/**
	 * Un-reads a character from the stream.
	 *
	 * @param codePoint
	 * 		the codePoint value to un-read back into the stream
	 *
	 * @return the codePoint un-read back into the stream
	 */
	Integer unreadChar(Integer codePoint);

	default CharacterStruct unreadChar(final CharacterStruct character) {
		unreadChar(character.toUnicodeCodePoint());
		return character;
	}

	/**
	 * Clears the input from the stream.
	 */
	void clearInput();

	default LispStruct clearInput1() {
		// TODO: Fix method name
		clearInput();
		return NILStruct.INSTANCE;
	}

	/**
	 * Listens on the stream to determine if there is any data left to read.
	 *
	 * @return whether or not there is data left to read from the stream
	 */
	boolean listen();

	default BooleanStruct listen1() {
		final boolean listen = listen();
		return BooleanStruct.toLispBoolean(listen);
	}

	@Override
	default boolean isInputStream() {
		return true;
	}

	default ReadLineResult readLine(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {

		final StringBuilder stringBuilder = new StringBuilder();

		ReadPeekResult readPeekResult = readChar(eofErrorP, eofValue, recursiveP);
		Integer result = readPeekResult.getResult();
		while (!readPeekResult.isEof() && (result != '\n')) {
			stringBuilder.appendCodePoint(result);

			readPeekResult = readChar(eofErrorP, eofValue, recursiveP);
			result = readPeekResult.getResult();
		}
		final String resultString = stringBuilder.toString();
		return new ReadLineResult(resultString, readPeekResult.isEof());
	}

	default LispStruct readLine(final BooleanStruct eofErrorP,
	                            final LispStruct eofValue,
	                            final BooleanStruct recursiveP) {

		final ReadLineResult readLineResult = readLine(eofErrorP.toJavaPBoolean(), eofValue,
		                                               recursiveP.toJavaPBoolean());
		final String result = readLineResult.getResult();
		final boolean eof = readLineResult.isEof();
		return ValuesStruct.valueOf(StringStruct.toLispString(result), BooleanStruct.toLispBoolean(eof));
	}
}
