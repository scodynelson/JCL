package jcl.lang;

import java.net.URL;

import jcl.lang.internal.stream.URLStreamStructImpl;

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

	static URLStreamStruct toURLStream(final URL url) {
		return new URLStreamStructImpl(url);
	}
}
