package jcl.lang;

/**
 * The {@link SynonymStreamStruct} is the object representation of a Lisp 'synonym-stream' type.
 */
public interface SynonymStreamStruct extends IOStreamStruct {

	/**
	 * Getter for synonym-stream {@link SymbolStructImpl} property.
	 *
	 * @return synonym-stream {@link SymbolStructImpl} property
	 */
	SymbolStructImpl getSymbol();
}
