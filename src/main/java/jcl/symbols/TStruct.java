package jcl.symbols;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link TStruct} is the object representation of a Lisp 't' type.
 */
public final class TStruct extends BooleanStruct {

	public static final TStruct INSTANCE = new TStruct();

	private static final long serialVersionUID = -22807328848501661L;

	/**
	 * Private constructor.
	 */
	private TStruct() {
		super("T", true);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
