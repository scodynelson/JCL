package jcl.lang;

/**
 * The {@link BooleanStruct} is the object representation of a Lisp 'boolean' type.
 */
public interface BooleanStruct extends SymbolStruct {

	/**
	 * Returns the {@literal boolean} primitive equivalent for this structure.
	 *
	 * @return the {@literal boolean} primitive equivalent for this structure
	 */
	boolean toJavaPBoolean();

	/**
	 * Returns the {@link Boolean} equivalent for this structure.
	 *
	 * @return the {@link Boolean} equivalent for this structure
	 */
	default Boolean toJavaBoolean() {
		return toJavaPBoolean();
	}

	/**
	 * Returns a new BooleanStruct representation of the provided {@literal boolean}.
	 *
	 * @param booleanValue
	 * 		the {@literal boolean} to represent as a BooleanStruct
	 *
	 * @return a new BooleanStruct representation of the provided {@literal boolean}
	 */
	static BooleanStruct toLispBoolean(final boolean booleanValue) {
		return booleanValue ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}
}
