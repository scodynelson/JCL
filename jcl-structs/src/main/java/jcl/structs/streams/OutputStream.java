package jcl.structs.streams;

import jcl.structs.conditions.exceptions.StreamErrorException;

public interface OutputStream extends LispStream {

	void writeChar(int aChar) throws StreamErrorException;

	void writeByte(int aByte) throws StreamErrorException;

	void writeString(String outputString, int start, int end) throws StreamErrorException;

	void clearOutput();

	void finishOutput();

	void forceOutput();
}
