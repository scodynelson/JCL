package jcl.lang;

import java.net.URL;

/**
 * The {@link URLStreamStruct} is the object representation of a Lisp 'url-stream' type.
 */
public interface URLStreamStruct extends IOStreamStruct {

	/**
	 * Getter for the {@link URL} value.
	 *
	 * @return the {@link URL} value
	 */
	URL getUrl();
}
