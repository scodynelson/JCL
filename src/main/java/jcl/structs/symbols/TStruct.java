package jcl.structs.symbols;

/**
 * The {@code TStruct} is the object representation of a Lisp 't' type.
 */
public final class TStruct extends BooleanStruct<TStruct> {

	public static final TStruct INSTANCE = new TStruct();

	/**
	 * Private constructor.
	 */
	private TStruct() {
		super("T", true);
	}
}
