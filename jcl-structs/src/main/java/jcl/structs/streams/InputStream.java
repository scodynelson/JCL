package jcl.structs.streams;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.LispType;

public interface InputStream extends LispStream {

	ReadResult readChar(boolean eofErrorP, LispStruct eofValue, boolean recursiveP) throws StreamErrorException;

	ReadResult readByte(boolean eofErrorP, LispStruct eofValue) throws StreamErrorException;

	PeekResult peekChar(LispType peekType, boolean eofErrorP, LispStruct eofValue, boolean recursiveP) throws StreamErrorException;

	Integer unreadChar(Integer codePoint) throws StreamErrorException;

	void clearInput();

	boolean listen();
}
