package jcl.lang;

/**
 * The {@link StringOutputStreamStruct} is the object representation of a Lisp 'string-stream' output type.
 */
public interface StringOutputStreamStruct extends OutputStreamStruct {

	String getStreamString();

	void clearStream();
}
