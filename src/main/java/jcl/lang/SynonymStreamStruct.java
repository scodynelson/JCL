package jcl.lang;

import jcl.lang.internal.stream.SynonymStreamStructImpl;

/**
 * The {@link SynonymStreamStruct} is the object representation of a Lisp 'synonym-stream' type.
 */
public interface SynonymStreamStruct extends IOStreamStruct {

	/**
	 * Returns the {@link SymbolStruct} stream symbol.
	 *
	 * @return the {@link SymbolStruct} stream symbol
	 */
	SymbolStruct synonymStreamSymbol();

	/**
	 * Returns a new Synonym-Stream instance that will delegate stream operations to the value of the provided {@link
	 * SymbolStruct}.
	 *
	 * @param symbol
	 * 		the {@link SymbolStruct} containing a {@link StreamStruct} value
	 *
	 * @return a new Synonym-Stream instance
	 */
	static SynonymStreamStruct toSynonymStream(final SymbolStruct symbol) {
		return new SynonymStreamStructImpl(symbol);
	}
}
