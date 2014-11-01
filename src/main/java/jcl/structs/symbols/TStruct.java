package jcl.structs.symbols;

import jcl.types.T;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

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

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
