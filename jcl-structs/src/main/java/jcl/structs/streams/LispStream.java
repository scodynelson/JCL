package jcl.structs.streams;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.LispType;

public interface LispStream extends LispStruct {

	void close() throws StreamErrorException;

	LispType elementType();

	Long fileLength() throws StreamErrorException;

	Long filePosition(Long filePosition) throws StreamErrorException;

	boolean isInteractive();

	boolean isClosed();
}
