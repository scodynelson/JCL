package jcl.structs.symbols;

/**
 * The {@code NILStruct} is the object representation of a Lisp 'nil' type.
 */
public final class NILStruct extends BooleanStruct<NILStruct> {

	public static final NILStruct INSTANCE = new NILStruct();

	/**
	 * Private constructor.
	 */
	private NILStruct() {
		super("NIL", false);
	}
}
