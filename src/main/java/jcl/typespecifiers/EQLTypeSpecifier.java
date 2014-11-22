package jcl.typespecifiers;

import jcl.LispType;
import jcl.types.TypeBaseClass;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * An {@link EQLTypeSpecifier} represents the type of all x for which (eql object x) is true. The argument object is
 * required. The object can be *, but if so it denotes itself (the symbol *) and does not represent an unspecified
 * value. The symbol eql is not valid as an atomic type specifier.
 */
public class EQLTypeSpecifier extends TypeBaseClass implements CompoundTypeSpecifier {

	private final TypeSpecifier typeSpecifier;

	/**
	 * Public constructor.
	 *
	 * @param typeSpecifier
	 * 		the type specifier to test equality
	 */
	public EQLTypeSpecifier(final TypeSpecifier typeSpecifier) {
		this("T", typeSpecifier); // TODO: Should this be 'T'???
	}

	/**
	 * Protected constructor.
	 *
	 * @param name
	 * 		the name of the symbol type
	 * @param typeSpecifier
	 * 		the type specifier to test equality
	 */
	protected EQLTypeSpecifier(final String name, final TypeSpecifier typeSpecifier) {
		super(name);
		this.typeSpecifier = typeSpecifier;
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if (!(obj instanceof LispType)) {
			return false;
		}

		// TODO: does this account for x.equals(y) and y.equals(x)???
		final LispType lispType = (LispType) obj;
		return typeSpecifier.equals(lispType);
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
