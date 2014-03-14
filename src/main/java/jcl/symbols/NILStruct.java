package jcl.symbols;

import jcl.classes.BuiltInClassStruct;
import jcl.types.NIL;

/**
 * The {@code NILStruct} is the object representation of a Lisp 'nil' type.
 */
public final class NILStruct extends BuiltInClassStruct {

	public static final NILStruct INSTANCE = new NILStruct();

	/**
	 * Private constructor.
	 */
	private NILStruct() {
		super(NIL.INSTANCE, null, null);
	}
}
