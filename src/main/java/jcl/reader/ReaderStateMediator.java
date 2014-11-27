/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

interface ReaderStateMediator {

	void read(Reader reader, TokenBuilder tokenBuilder);

	void readIllegalCharacter(Reader reader, TokenBuilder tokenBuilder);

	void readWhitespace(Reader reader, TokenBuilder tokenBuilder);

	void readMacroCharacter(Reader reader, TokenBuilder tokenBuilder);

	void readSingleEscape(Reader reader, TokenBuilder tokenBuilder);

	void readMultipleEscape(Reader reader, TokenBuilder tokenBuilder);

	void readConstituent(Reader reader, TokenBuilder tokenBuilder);

	void readEvenMultipleEscape(Reader reader, TokenBuilder tokenBuilder);

	void readOddMultipleEscape(Reader reader, TokenBuilder tokenBuilder);

	void readTokenAccumulated(Reader reader, TokenBuilder tokenBuilder);
}
