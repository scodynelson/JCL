package jcl.streams;

import jcl.LispStruct;
import jcl.LispType;

/**
 * The {@code LispStream} is the representation for all Lisp 'stream' types.
 */
public interface LispStream extends LispStruct {

	/**
	 * This method closes the stream.
	 */
	void close();

	/**
	 * This method returns the element type of the stream.
	 *
	 * @return the element type of the stream
	 */
	LispType getElementType();

	/**
	 * This method returns the length of the stream if it is a {@code FileStreamStruct}.
	 *
	 * @return the length of the stream
	 */
	Long fileLength();

	/**
	 * This method returns the current position in the stream if it is a {@code FileStreamStruct}.
	 *
	 * @param filePosition if not null, the current stream position will be set to this value
	 * @return the current position in the stream
	 */
	Long filePosition(Long filePosition);

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
