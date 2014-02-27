package jcl.structs.streams;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.LispType;

/**
 * The {@code LispStream} is the representation for all Lisp 'stream' types.
 */
public interface LispStream extends LispStruct {

	/**
	 * This method closes the stream.
	 *
	 * @throws StreamErrorException if the stream could not be closed
	 */
	void close() throws StreamErrorException;

	/**
	 * This method returns the element type of the stream.
	 *
	 * @return the element type of the stream
	 */
	LispType elementType();

	/**
	 * This method returns the length of the stream if it is a {@code FileStreamStruct}.
	 *
	 * @return the length of the stream
	 * @throws StreamErrorException if the stream is not a {@code FileStreamStruct} or the length could not be obtained
	 */
	Long fileLength() throws StreamErrorException;

	/**
	 * This method returns the current position in the stream if it is a {@code FileStreamStruct}.
	 *
	 * @param filePosition if not null, the current stream position will be set to this value
	 * @return the current position in the stream
	 * @throws StreamErrorException if the stream is not a {@code FileStreamStruct} or the position could not be obtained
	 */
	Long filePosition(Long filePosition) throws StreamErrorException;

	/**
	 * This method returns whether or not the stream is interactive.
	 *
	 * @return whether or not the stream is interactive
	 */
	boolean isInteractive();

	/**
	 * This method returns whether or not the stream is closed.
	 *
	 * @return whether or not the stream is closed
	 */
	boolean isClosed();
}
