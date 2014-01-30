package jcl.structs.streams;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.EndOfFileException;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.LispType;
import jcl.types.T;
import jcl.types.streams.ConcatenatedStream;

import java.util.Arrays;
import java.util.LinkedList;

/**
 * The {@code ConcatenatedStreamStruct} is the object representation of a Lisp 'concatenated-stream' type.
 */
public class ConcatenatedStreamStruct extends StreamStruct implements InputStream {

	private final LinkedList<InputStream> inputStreams;

	private final boolean isInteractive;
	private boolean isClosed;

	/**
	 * Private constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param inputStreams  the {@code InputStream}s to create a {@code ConcatenatedStreamStruct} from
	 * @throws StreamErrorException if the struct cannot be created
	 */
	private ConcatenatedStreamStruct(final boolean isInteractive, final LinkedList<InputStream> inputStreams) throws StreamErrorException {
		this.isInteractive = isInteractive;

		if (inputStreams == null) {
			throw new StreamErrorException("Provided Input Stream List must not be null.");
		}
		this.inputStreams = new LinkedList<>(inputStreams);
	}

	@Override
	public LispType getType() {
		return ConcatenatedStream.INSTANCE;
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws StreamErrorException {
		while (true) {
			if (inputStreams.isEmpty()) {
				if (eofErrorP) {
					throw new EndOfFileException("End of file reached.");
				} else {
					return new ReadResult(eofValue);
				}
			}

			final InputStream first = inputStreams.getFirst();

			final ReadResult readResult = first.readChar(false, eofValue, false);
			if (readResult.wasEOF()) {
				inputStreams.removeFirst();
			} else {
				return readResult;
			}
		}
	}

	@Override
	public ReadResult readByte(final boolean eofErrorP, final LispStruct eofValue) throws StreamErrorException {
		while (true) {
			if (inputStreams.isEmpty()) {
				if (eofErrorP) {
					throw new EndOfFileException("End of file reached.");
				} else {
					return new ReadResult(eofValue);
				}
			}

			final InputStream first = inputStreams.getFirst();

			final ReadResult readResult = first.readByte(false, eofValue);
			if (readResult.wasEOF()) {
				inputStreams.removeFirst();
			} else {
				return readResult;
			}
		}
	}

	@Override
	public PeekResult peekChar(final LispType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws StreamErrorException {
		if (inputStreams.isEmpty()) {
			if (eofErrorP) {
				throw new EndOfFileException("End of file reached.");
			} else {
				return new PeekResult(eofValue);
			}
		}

		final InputStream first = inputStreams.getFirst();
		return first.peekChar(peekType, eofErrorP, eofValue, recursiveP);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) throws StreamErrorException {
		if (inputStreams.isEmpty()) {
			return null;
		}

		final InputStream first = inputStreams.getFirst();
		return first.unreadChar(codePoint);
	}

	@Override
	public void clearInput() {
		if (!inputStreams.isEmpty()) {
			final InputStream first = inputStreams.getFirst();
			first.clearInput();
		}
	}

	@Override
	public boolean listen() {
		if (inputStreams.isEmpty()) {
			return false;
		}

		while (true) {
			final InputStream first = inputStreams.getFirst();
			final boolean listen = first.listen();
			if (listen) {
				return true;
			}

			inputStreams.removeFirst();
			if (inputStreams.isEmpty()) {
				return false;
			}
		}
	}

	@Override
	public void close() throws StreamErrorException {
		isClosed = true;
	}

	@Override
	public LispType elementType() {
		if (inputStreams.isEmpty()) {
			return T.INSTANCE;
		}

		final InputStream last = inputStreams.getLast();
		return last.elementType();
	}

	@Override
	public Long fileLength() throws StreamErrorException {
		if (inputStreams.isEmpty()) {
			return 0L;
		}

		final InputStream last = inputStreams.getLast();
		return last.fileLength();
	}

	@Override
	public Long filePosition(final Long filePosition) throws StreamErrorException {
		if (inputStreams.isEmpty()) {
			return 0L;
		}

		final InputStream last = inputStreams.getLast();
		return last.filePosition(filePosition);
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
		return "ConcatenatedStreamStruct{" +
				"inputStreams=" + inputStreams +
				", isInteractive=" + isInteractive +
				", isClosed=" + isClosed +
				'}';
	}

	// BUILDERS

	/**
	 * This method gets the {@code ConcatenatedStreamStruct} for the provided {@code inputStreams}.
	 *
	 * @param inputStreams the {@code InputStream}s to create a {@code ConcatenatedStreamStruct} from
	 * @return the created {@code ConcatenatedStreamStruct}
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public static ConcatenatedStreamStruct getStruct(final InputStream... inputStreams) throws StreamErrorException {
		return new ConcatenatedStreamStruct(false, new LinkedList<>(Arrays.asList(inputStreams)));
	}

	/**
	 * This method gets the {@code ConcatenatedStreamStruct} for the provided {@code inputStreams}.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param inputStreams  the {@code InputStream}s to create a {@code ConcatenatedStreamStruct} from
	 * @return the created {@code ConcatenatedStreamStruct}
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public static ConcatenatedStreamStruct getStruct(final boolean isInteractive, final InputStream... inputStreams) throws StreamErrorException {
		return new ConcatenatedStreamStruct(isInteractive, new LinkedList<>(Arrays.asList(inputStreams)));
	}

	/**
	 * This method gets the {@code ConcatenatedStreamStruct} for the provided {@code inputStreams}.
	 *
	 * @param inputStreams the {@code InputStream}s to create a {@code ConcatenatedStreamStruct} from
	 * @return the created {@code ConcatenatedStreamStruct}
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public static ConcatenatedStreamStruct getStruct(final LinkedList<InputStream> inputStreams) throws StreamErrorException {
		return new ConcatenatedStreamStruct(false, inputStreams);
	}

	/**
	 * This method gets the {@code ConcatenatedStreamStruct} struct for the provided {@code inputStreams}.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param inputStreams  the {@code InputStream}s to create a {@code ConcatenatedStreamStruct} from
	 * @return the created {@code ConcatenatedStreamStruct}
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public static ConcatenatedStreamStruct getStruct(final boolean isInteractive, final LinkedList<InputStream> inputStreams) throws StreamErrorException {
		return new ConcatenatedStreamStruct(isInteractive, inputStreams);
	}
}
