package jcl.symbols;

/**
 * The {@link NILStruct} is the object representation of a Lisp 'nil' type.
 */
public final class NILStruct extends BooleanStruct {

	public static final NILStruct INSTANCE = new NILStruct();

	private static final long serialVersionUID = 8370329377005791641L;

	/**
	 * Private constructor.
	 */
	private NILStruct() {
		super("NIL", false);
	}
}
