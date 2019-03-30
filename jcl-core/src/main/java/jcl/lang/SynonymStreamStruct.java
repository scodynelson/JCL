package jcl.lang;

import jcl.lang.internal.stream.SynonymStreamStructImpl;

/**
 * The {@link SynonymStreamStruct} is the object representation of a Lisp 'synonym-stream' type.
 */
public interface SynonymStreamStruct extends IOStreamStruct {

	/**
	 * Getter for synonym-stream {@link SymbolStruct} property.
	 *
	 * @return synonym-stream {@link SymbolStruct} property
	 */
	SymbolStruct getSymbol();

	default SymbolStruct synonymStreamSymbol() {
		return getSymbol();
	}

	static SynonymStreamStruct toSynonymStream(final SymbolStruct symbol) {
		return new SynonymStreamStructImpl(symbol);
	}
}
