package jcl.streams;

import jcl.LispType;
import jcl.conditions.exceptions.EndOfFileException;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.syntax.reader.PeekResult;
import jcl.syntax.reader.PeekType;
import jcl.types.Stream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link NativeStreamStruct} is an abstraction for native stream types.
 */
abstract class NativeStreamStruct extends StreamStruct implements InputStream, OutputStream {

	private static final Logger LOGGER = LoggerFactory.getLogger(NativeStreamStruct.class);

	/**
	 * Protected constructor.
	 *
	 * @param type          the type of the stream object
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param elementType   the stream elementType
	 */
	protected NativeStreamStruct(final Stream type,
								 final boolean isInteractive, final LispType elementType) {
		super(type, null, null, isInteractive, elementType);
	}

	@Override
	public boolean listen() {
		try {
			final PeekResult peekResult = peekChar(PeekType.NIL_PEEK_TYPE, false, null, false);
			return !peekResult.wasEOF();
		} catch (final EndOfFileException eofe) {
			LOGGER.warn(StreamUtils.END_OF_FILE_REACHED, eofe);
			return false;
		} catch (final StreamErrorException see) {
			LOGGER.warn("Stream error occurred.", see);
			return false;
		}
	}
}
