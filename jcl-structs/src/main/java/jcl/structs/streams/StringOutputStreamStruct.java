package jcl.structs.streams;

import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.characters.BaseChar;
import jcl.types.streams.StringStream;

/**
 * The {@code StringOutputStreamStruct} is the object representation of a Lisp 'string-stream' output type.
 */
public class StringOutputStreamStruct extends StreamStruct implements OutputStream {

	private final StringBuffer stringBuffer = new StringBuffer();
	private int index;

	/**
	 * Public constructor.
	 */
	public StringOutputStreamStruct() {
		this(false);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 */
	public StringOutputStreamStruct(final boolean isInteractive) {
		super(StringStream.INSTANCE, null, null, isInteractive, BaseChar.INSTANCE);
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
	public String toString() {
		return "StringOutputStreamStruct{" +
				"stringBuffer=" + stringBuffer +
				", index=" + index +
				'}';
	}
}
