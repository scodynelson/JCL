package jcl.compiler.old.functions;

import jcl.lists.NullStruct;
import jcl.streams.StreamStruct;
import jcl.symbols.TStruct;

/**
 * Implementation of the Lisp Function CLOSE which ends the association between
 * file/stream and the top-level.  In the current implementation, only file streams
 * can be closed.  Files can be closed multiple times with no ill effects.
 */
public class CloseFunction {

	public static final CloseFunction FUNCTION = new CloseFunction();

	/**
	 * Close called with one argument.  To be successful, the one argument must
	 * be a stream -- open or closed.
	 *
	 * @param arg1 the stream to be closed
	 * @return the value T
	 */
	public Object funcall(Object arg1) {
		return funcall(arg1, NullStruct.INSTANCE);
	}

	/**
	 * The Close function called with the file stream and keyword/modifier pair
	 * for ABORT.
	 *
	 * @param arg1 the stream to be closed
	 * @param arg2 modifier for ABORT
	 * @return the value T
	 */
	public Object funcall(Object arg1, Object arg2) {
		StreamStruct fileStream;
		if (arg1 instanceof StreamStruct) {
			fileStream = (StreamStruct) arg1;
			fileStream.close();
		} else {
			throw new RuntimeException(arg1 + " is not an valid stream.");
		}
		return TStruct.INSTANCE;
	}
}
