package jcl.structs.symbols;

import jcl.types.NIL;

/**
 * The {@link NILStruct} is the object representation of a Lisp 'nil' type.
 */
public final class NILStruct extends BooleanStruct<NIL> {

	public static final NILStruct INSTANCE = new NILStruct();

	/**
	 * Private constructor.
	 */
	private NILStruct() {
		super("NIL", NIL.INSTANCE, false);
	}
}
