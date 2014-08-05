package jcl.structs.symbols;

import jcl.types.T;

/**
 * The {@link TStruct} is the object representation of a Lisp 't' type.
 */
public final class TStruct extends BooleanStruct<T> {

	public static final TStruct INSTANCE = new TStruct();

	/**
	 * Private constructor.
	 */
	private TStruct() {
		super("T", T.INSTANCE, true);
	}
}
