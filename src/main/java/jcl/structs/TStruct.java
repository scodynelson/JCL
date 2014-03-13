package jcl.structs;

import jcl.types.T;

/**
 * The {@code TStruct} is the object representation of a Lisp 't' type.
 */
public final class TStruct extends BuiltInClassStruct {

	public static final TStruct INSTANCE = new TStruct();

	/**
	 * Private constructor.
	 */
	private TStruct() {
		super(T.INSTANCE, null, null);
	}
}
