package jcl.lang.condition.exception;

/**
 * The class {@link ReaderErrorException} is a form of {@link Throwable} that
 * indicates exception conditions within the reader functionality.
 */
public class ReaderErrorException extends ParseErrorException {

	private static final long serialVersionUID = 8974486019161389585L;

	/**
	 * Constructs a new exception with the specified detail message.  The
	 * cause is not initialized, and may subsequently be initialized by
	 * a call to {@link #initCause}.
	 *
	 * @param message
	 * 		the detail message. The detail message is saved for
	 * 		later retrieval by the {@link #getMessage()} method.
	 */
	public ReaderErrorException(final String message) {
		super(message);
	}

	/**
	 * Constructs a new exception with the specified detail message and
	 * cause.  <p>Note that the detail message associated with
	 * {@code cause} is <i>not</i> automatically incorporated in
	 * this exception's detail message.
	 *
	 * @param message
	 * 		the detail message (which is saved for later retrieval
	 * 		by the {@link #getMessage()} method).
	 * @param cause
	 * 		the cause (which is saved for later retrieval by the
	 * 		{@link #getCause()} method).  (A <tt>null</tt> value is
	 * 		permitted, and indicates that the cause is nonexistent or
	 * 		unknown.)
	 */
	public ReaderErrorException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
