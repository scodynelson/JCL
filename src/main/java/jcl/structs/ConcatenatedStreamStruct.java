package jcl.structs;

import jcl.LispStruct;
import jcl.structs.conditions.exceptions.EndOfFileException;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.structs.streams.PeekResult;
import jcl.structs.streams.PeekType;
import jcl.structs.streams.ReadResult;
import jcl.LispType;
import jcl.types.T;
import jcl.types.ConcatenatedStream;

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
	 */
	public ConcatenatedStreamStruct(final LinkedList<InputStream> inputStreams) {
		this(false, inputStreams);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param inputStreams  the {@code InputStream}s to create a {@code ConcatenatedStreamStruct} from
	 */
	public ConcatenatedStreamStruct(final boolean isInteractive, final LinkedList<InputStream> inputStreams) {
		super(ConcatenatedStream.INSTANCE, null, null, isInteractive, getElementType(inputStreams));
		this.inputStreams = new LinkedList<>(inputStreams);
	}

	/**
	 * This private method is used to retrieve the element type for object construction.
	 *
	 * @param inputStreams the {@code InputStream}s to create a {@code ConcatenatedStreamStruct} from
	 * @return the element type for object construction
	 */
	private static LispType getElementType(final LinkedList<InputStream> inputStreams) {
		if (inputStreams == null) {
			throw new StreamErrorException("Provided Input Stream List must not be null.");
		}
		return getElementType2(inputStreams);
	}

	/**
	 * This private method is used to retrieve the element type for object construction.
	 *
	 * @param inputStreams the {@code InputStream}s to create a {@code ConcatenatedStreamStruct} from
	 * @return the element type for object construction
	 */
	private static LispType getElementType2(final LinkedList<InputStream> inputStreams) {
		if (inputStreams.isEmpty()) {
			return T.INSTANCE;
		}

		final InputStream last = inputStreams.getLast();
		return last.getElementType();
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
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
	public ReadResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
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
	public PeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
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
	public Integer unreadChar(final Integer codePoint) {
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
	public LispType getElementType() {
		return getElementType2(inputStreams);
	}

	@Override
	public Long fileLength() {
		if (inputStreams.isEmpty()) {
			return 0L;
		}

		final InputStream last = inputStreams.getLast();
		return last.fileLength();
	}

	@Override
	public Long filePosition(final Long filePosition) {
		if (inputStreams.isEmpty()) {
			return 0L;
		}

		final InputStream last = inputStreams.getLast();
		return last.filePosition(filePosition);
	}

	@Override
	public String toString() {
		return "ConcatenatedStreamStruct{"
				+ "inputStreams=" + inputStreams
				+ '}';
	}
}
