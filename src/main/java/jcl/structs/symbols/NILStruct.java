package jcl.structs.symbols;

import jcl.types.NIL;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

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

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
