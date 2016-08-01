package jcl.lang;

/**
 * The {@link DualStreamStruct} is an abstraction for dual stream types.
 */
public interface DualStreamStruct extends IOStreamStruct {

	/**
	 * Getter for {@link InputStreamStruct} value.
	 *
	 * @return {@link InputStreamStruct} value
	 */
	InputStreamStruct getInputStreamStruct();

	/**
	 * Getter for {@link OutputStreamStruct} value.
	 *
	 * @return {@link OutputStreamStruct} value
	 */
	OutputStreamStruct getOutputStreamStruct();
}
