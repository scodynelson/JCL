package jcl.types.typespecifiers;

import jcl.lambdalist.variable.Optional;
import jcl.lambdalist.variable.Rest;
import jcl.types.TypeBaseClass;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.List;
import java.util.Objects;

/**
 * A {@link ValuesTypeSpecifier} can be used only as the value-type in a function type specifier or a the special form.
 * It is used to specify individual types when multiple values are involved. The &optional and &rest markers can appear
 * in the value-type list; they indicate the parameter list of a function that, when given to multiple-value-call along
 * with the values, would correctly receive those values. The symbol values is not valid as a type specifier; and,
 * specifically, it is not an abbreviation for (values).
 */
public class ValuesTypeSpecifier extends TypeBaseClass implements CompoundTypeSpecifier {

	private final List<TypeSpecifier> typeSpecifiers;
	private final Optional<TypeSpecifier> optional;
	private final Rest<TypeSpecifier> rest;

	/**
	 * Constructs a new ValuesTypeSpecifier that matches the provided typeSpecifiers, optional, and rest arguments.
	 *
	 * @param typeSpecifiers
	 * 		the required arguments
	 * @param optional
	 * 		the optional arguments
	 * @param rest
	 * 		the rest arguments
	 */
	public ValuesTypeSpecifier(final List<TypeSpecifier> typeSpecifiers, final Optional<TypeSpecifier> optional,
	                           final Rest<TypeSpecifier> rest) {
		this("T", typeSpecifiers, optional, rest); // TODO: Should this be 'T'???
	}

	/**
	 * Constructs a new ValuesTypeSpecifier that matches the provided typeSpecifiers, optional, and rest arguments.
	 *
	 * @param name
	 * 		the name of the symbol type
	 * @param typeSpecifiers
	 * 		the required arguments
	 * @param optional
	 * 		the optional arguments
	 * @param rest
	 * 		the rest arguments
	 */
	protected ValuesTypeSpecifier(final String name, final List<TypeSpecifier> typeSpecifiers, final Optional<TypeSpecifier> optional,
	                              final Rest<TypeSpecifier> rest) {
		super(name);
		this.typeSpecifiers = typeSpecifiers;
		this.optional = optional;
		this.rest = rest;
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if (!(obj instanceof ValuesTypeSpecifier)) {
			return false;
		}

		// TODO: does this account for x.equals(y) and y.equals(x)???
		final ValuesTypeSpecifier valuesTypeSpecifier = (ValuesTypeSpecifier) obj;
		return Objects.equals(typeSpecifiers, valuesTypeSpecifier.typeSpecifiers)
				&& Objects.equals(optional, valuesTypeSpecifier.optional)
				&& Objects.equals(rest, valuesTypeSpecifier.rest);
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
