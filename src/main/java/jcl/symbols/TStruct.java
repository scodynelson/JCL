package jcl.symbols;

import jcl.types.TType;

/**
 * The {@link TStruct} is the object representation of a Lisp 't' type.
 */
public final class TStruct extends BooleanStruct {

	public static final TStruct INSTANCE = new TStruct();

	/**
	 * Private constructor.
	 */
	private TStruct() {
		super(TType.INSTANCE, "T", true);
	}
}
