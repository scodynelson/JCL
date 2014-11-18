package jcl.structs.streams;

import jcl.LispStruct;
import jcl.structs.conditions.exceptions.EndOfFileException;
import jcl.types.EchoStream;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.LinkedList;

/**
 * The {@link EchoStreamStruct} is the object representation of a Lisp 'echo-stream' type.
 */
public class EchoStreamStruct extends DualStreamStruct {

	private final LinkedList<Integer> unreadStuff = new LinkedList<>();

	/**
	 * Public constructor.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} to create a EchoStreamStruct from
	 * @param outputStream
	 * 		the {@link OutputStream} to create a EchoStreamStruct from
	 */
	public EchoStreamStruct(final InputStream inputStream, final OutputStream outputStream) {
		this(false, inputStream, outputStream);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive
	 * 		whether or not the struct created is 'interactive'
	 * @param inputStream
	 * 		the {@link InputStream} to create a EchoStreamStruct from
	 * @param outputStream
	 * 		the {@link OutputStream} to create a EchoStreamStruct from
	 */
	public EchoStreamStruct(final boolean isInteractive, final InputStream inputStream, final OutputStream outputStream) {
		super(EchoStream.INSTANCE, isInteractive, inputStream, outputStream);
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (!unreadStuff.isEmpty()) {
			final Integer lastUnread = unreadStuff.getFirst();
			return new ReadResult(lastUnread);
		}

		final ReadResult readResult = inputStream.readChar(false, eofValue, false);

		if (readResult.wasEOF()) {
			if (eofErrorP) {
				throw new EndOfFileException(StreamUtils.END_OF_FILE_REACHED);
			} else {
				return readResult;
			}
		} else {
			final int readChar = readResult.getResult();
			outputStream.writeChar(readChar);
			return readResult;
		}
	}

	@Override
	public ReadResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		if (!unreadStuff.isEmpty()) {
			final Integer lastUnread = unreadStuff.getFirst();
			return new ReadResult(lastUnread);
		}

		final ReadResult readResult = inputStream.readByte(false, eofValue);

		if (readResult.wasEOF()) {
			if (eofErrorP) {
				throw new EndOfFileException(StreamUtils.END_OF_FILE_REACHED);
			} else {
				return readResult;
			}
		} else {
			final int readByte = readResult.getResult();
			outputStream.writeByte(readByte);
			return readResult;
		}
	}

	@Override
	public PeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (unreadStuff.isEmpty()) {
			final ReadResult readResult = inputStream.readChar(eofErrorP, eofValue, recursiveP);

			if (readResult.wasEOF()) {
				return new PeekResult(readResult.getEofValue());
			} else {
				final int peekedChar = readResult.getResult();
				outputStream.writeChar(peekedChar);
				return new PeekResult(peekedChar);
			}
		} else {
			final Integer peekedChar = unreadStuff.removeFirst();
			return new PeekResult(peekedChar);
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		unreadStuff.addFirst(codePoint);
		return codePoint;
	}

	@Override
	public Long filePosition(final Long filePosition) {
		return null;
	}

	@Override
	protected String getPrintableObjectProperties() {
		return " input " + inputStream.printStruct() + ", output " + outputStream.printStruct();
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
