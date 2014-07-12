package jcl.symbols;

/**
 * The {@code TStruct} is the object representation of a Lisp 't' type.
 */
public final class TStruct extends SymbolStruct<TStruct> {

	public static final TStruct INSTANCE = new TStruct();

	/**
	 * Private constructor.
	 */
	private TStruct() {
		super("T");
	}
}
