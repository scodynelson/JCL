package jcl.structs.streams;

import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.LispType;
import jcl.types.T;
import jcl.types.streams.BroadcastStream;

import java.util.LinkedList;

/**
 * The {@code BroadcastStreamStruct} is the object representation of a Lisp 'broadcast-stream' type.
 */
public class BroadcastStreamStruct extends StreamStruct implements OutputStream {

	private final LinkedList<OutputStream> outputStreams;

	/**
	 * Public constructor.
	 *
	 * @param outputStreams the {@code OutputStream}s to create a {@code BroadcastStreamStruct} from
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public BroadcastStreamStruct(final LinkedList<OutputStream> outputStreams) throws StreamErrorException {
		this(false, outputStreams);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param outputStreams the {@code OutputStream}s to create a {@code BroadcastStreamStruct} from
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public BroadcastStreamStruct(final boolean isInteractive, final LinkedList<OutputStream> outputStreams) throws StreamErrorException {
		super(BroadcastStream.INSTANCE, null, null, isInteractive, getElementType(outputStreams));

		if (outputStreams == null) {
			throw new StreamErrorException("Provided Output Stream List must not be null.");
		}
		this.outputStreams = new LinkedList<>(outputStreams);
	}

	private static LispType getElementType(final LinkedList<OutputStream> outputStreams) {
		if (outputStreams.isEmpty()) {
			return T.INSTANCE;
		}

		final OutputStream last = outputStreams.getLast();
		return last.elementType();
	}

	@Override
	public LispType getType() {
		return BroadcastStream.INSTANCE;
	}

	@Override
	public void writeChar(final int aChar) throws StreamErrorException {
		for (final OutputStream outputStream : outputStreams) {
			outputStream.writeChar(aChar);
		}
	}

	@Override
	public void writeByte(final int aByte) throws StreamErrorException {
		for (final OutputStream outputStream : outputStreams) {
			outputStream.writeByte(aByte);
		}
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) throws StreamErrorException {
		for (final OutputStream outputStream : outputStreams) {
			outputStream.writeString(outputString, start, end);
		}
	}

	@Override
	public void clearOutput() {
		for (final OutputStream outputStream : outputStreams) {
			outputStream.clearOutput();
		}
	}

	@Override
	public void finishOutput() {
		for (final OutputStream outputStream : outputStreams) {
			outputStream.finishOutput();
		}
	}

	@Override
	public void forceOutput() {
		for (final OutputStream outputStream : outputStreams) {
			outputStream.forceOutput();
		}
	}

	@Override
	public LispType elementType() {
		return getElementType(outputStreams);
	}

	@Override
	public Long fileLength() throws StreamErrorException {
		if (outputStreams.isEmpty()) {
			return 0L;
		}

		final OutputStream last = outputStreams.getLast();
		return last.fileLength();
	}

	@Override
	public Long filePosition(final Long filePosition) throws StreamErrorException {
		if (outputStreams.isEmpty()) {
			return 0L;
		}

		final OutputStream last = outputStreams.getLast();
		return last.filePosition(filePosition);
	}

	@Override
	public String toString() {
		return "BroadcastStreamStruct{" +
				"outputStreams=" + outputStreams +
				'}';
	}
}
