package jcl.lang;

import jcl.lang.internal.KeywordStructImpl;

/**
 * The {@link KeywordStruct} is the object representation of a Lisp 'keyword' type.
 */
public interface KeywordStruct extends SymbolStruct {

	static KeywordStruct toLispKeyword(final String name) {
		return new KeywordStructImpl(name);
	}
}
