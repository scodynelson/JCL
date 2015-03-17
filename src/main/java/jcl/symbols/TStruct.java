package jcl.symbols;

/**
 * The {@link TStruct} is the object representation of a Lisp 't' type.
 */
public final class TStruct extends BooleanStruct {

	public static final TStruct INSTANCE = new TStruct();

	private static final long serialVersionUID = -22807328848501661L;

	/**
	 * Private constructor.
	 */
	private TStruct() {
		super("T", true);
	}
}
