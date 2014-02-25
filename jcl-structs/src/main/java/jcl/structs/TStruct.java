package jcl.structs;

import jcl.structs.classes.BuiltInClassStruct;
import jcl.types.T;

/**
 * The {@code TStruct} is the object representation of a Lisp 't' type.
 */
public class TStruct extends BuiltInClassStruct {

	public static final TStruct INSTANCE = new TStruct();

	/**
	 * Private constructor.
	 */
	private TStruct() {
		super(T.INSTANCE, null, null);
	}
}
