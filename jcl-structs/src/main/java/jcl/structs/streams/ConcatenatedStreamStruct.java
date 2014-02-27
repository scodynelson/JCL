package jcl.structs.streams;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.EndOfFileException;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.LispType;
import jcl.types.T;
import jcl.types.streams.ConcatenatedStream;

import java.util.LinkedList;

/**
 * The {@code ConcatenatedStreamStruct} is the object representation of a Lisp 'concatenated-stream' type.
 */
public class ConcatenatedStreamStruct extends StreamStruct implements InputStream {

	private final LinkedList<InputStream> inputStreams;

	/**
	 * Public constructor.
	 *
	 * @param inputStreams the {@code InputStream}s to create a {@code ConcatenatedStreamStruct} from
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public ConcatenatedStreamStruct(final LinkedList<InputStream> inputStreams) throws StreamErrorException {
		this(false, inputStreams);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param inputStreams  the {@code InputStream}s to create a {@code ConcatenatedStreamStruct} from
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public ConcatenatedStreamStruct(final boolean isInteractive, final LinkedList<InputStream> inputStreams) throws StreamErrorException {
		super(ConcatenatedStream.INSTANCE, null, null, isInteractive, getElementType(inputStreams));
		this.inputStreams = new LinkedList<>(inputStreams);
	}

	/**
	 * This private method is used to retrieve the element type for object construction.
	 *
	 * @param inputStreams the {@code InputStream}s to create a {@code ConcatenatedStreamStruct} from
	 * @return the element type for object construction
	 * @throws StreamErrorException if the element type cannot be retrieved
	 */
	private static LispType getElementType(final LinkedList<InputStream> inputStreams) throws StreamErrorException {
		if (inputStreams == null) {
			throw new StreamErrorException("Provided Input Stream List must not be null.");
		}
		return getElementType_2(inputStreams);
	}

	/**
	 * This private method is used to retrieve the element type for object construction.
	 *
	 * @param inputStreams the {@code InputStream}s to create a {@code ConcatenatedStreamStruct} from
	 * @return the element type for object construction
	 */
	private static LispType getElementType_2(final LinkedList<InputStream> inputStreams) {
		if (inputStreams.isEmpty()) {
			return T.INSTANCE;
		}

		final InputStream last = inputStreams.getLast();
		return last.elementType();
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
	public PeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws StreamErrorException {
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
	public LispType elementType() {
		return getElementType_2(inputStreams);
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
	public String toString() {
		return "ConcatenatedStreamStruct{" +
				"inputStreams=" + inputStreams +
				'}';
	}
}
