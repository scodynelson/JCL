package jcl.lang;

import java.nio.file.Path;

import jcl.lang.stream.ExternalFormat;

/**
 * The {@link FileStreamStruct} is the object representation of a Lisp 'file-stream' type.
 */
public interface FileStreamStruct extends IOStreamStruct {

	/**
	 * Getter for the {@link Path} value.
	 *
	 * @return the {@link Path} value
	 */
	Path getPath();

	ExternalFormat getExternalFormat();
}
