package jcl.symbols;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

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

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
