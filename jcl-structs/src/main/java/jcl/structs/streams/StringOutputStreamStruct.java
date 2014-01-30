package jcl.structs.streams;

import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.LispType;
import jcl.types.characters.BaseChar;
import jcl.types.streams.StringStream;

/**
 * The {@code StringOutputStreamStruct} is the object representation of a Lisp 'string-stream' output type.
 */
public class StringOutputStreamStruct extends StreamStruct implements OutputStream {

	private final StringBuffer stringBuffer = new StringBuffer();
	private int index;

	private final boolean isInteractive;
	private boolean isClosed;

	/**
	 * Private constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 */
	private StringOutputStreamStruct(final boolean isInteractive) {
		this.isInteractive = isInteractive;
	}

	@Override
	public LispType getType() {
		return StringStream.INSTANCE;
	}

	@Override
	public void writeChar(final int aChar) throws StreamErrorException {
		stringBuffer.appendCodePoint(aChar);
		index++;
	}

	@Override
	public void writeByte(final int aByte) throws StreamErrorException {
		throw new StreamErrorException("Operation only supported for BinaryStreams.");
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) throws StreamErrorException {
		final String subString = outputString.substring(start, end);
		stringBuffer.append(subString);
		index = stringBuffer.length();
	}

	@Override
	public void clearOutput() {
	}

	@Override
	public void finishOutput() {
	}

	@Override
	public void forceOutput() {
	}

	@Override
	public void close() throws StreamErrorException {
		isClosed = true;
	}

	@Override
	public LispType elementType() {
		return BaseChar.INSTANCE;
	}

	@Override
	public Long fileLength() throws StreamErrorException {
		throw new StreamErrorException("Operation only supported on a FileStream.");
	}

	@Override
	public Long filePosition(final Long filePosition) throws StreamErrorException {
		if (filePosition == null) {
			return (long) index;
		}
		return null;
	}

	@Override
	public boolean isInteractive() {
		return !isClosed && isInteractive;
	}

	@Override
	public boolean isClosed() {
		return isClosed;
	}

	@Override
	public String toString() {
		return "StringOutputStreamStruct{" +
				"stringBuffer=" + stringBuffer +
				", index=" + index +
				", isInteractive=" + isInteractive +
				", isClosed=" + isClosed +
				'}';
	}

	// BUILDERS

	/**
	 * This method gets a {@code StringOutputStreamStruct} struct.
	 *
	 * @return the created {@code StringOutputStreamStruct}
	 */
	public static StringOutputStreamStruct getStruct() {
		return new StringOutputStreamStruct(false);
	}

	/**
	 * This method gets a {@code StringOutputStreamStruct} struct.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @return the created {@code StringOutputStreamStruct}
	 */
	public static StringOutputStreamStruct getStruct(final boolean isInteractive) {
		return new StringOutputStreamStruct(isInteractive);
	}
}
