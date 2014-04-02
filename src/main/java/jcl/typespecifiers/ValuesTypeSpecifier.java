package jcl.typespecifiers;

import jcl.lambdalist.variable.Optional;
import jcl.lambdalist.variable.Rest;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.util.List;
import java.util.Objects;

/**
 * A {@code ValuesTypeSpecifier} can be used only as the value-type in a function type specifier or a the special form.
 * It is used to specify individual types when multiple values are involved. The &optional and &rest markers can appear
 * in the value-type list; they indicate the parameter list of a function that, when given to multiple-value-call along
 * with the values, would correctly receive those values. The symbol values is not valid as a type specifier; and, specifically,
 * it is not an abbreviation for (values).
 */
public class ValuesTypeSpecifier implements CompoundTypeSpecifier {

	private final List<TypeSpecifier> typeSpecifiers;
	private final Optional<TypeSpecifier> optional;
	private final Rest<TypeSpecifier> rest;

	/**
	 * Constructs a new {@code ValuesTypeSpecifier} that matches the provided typeSpecifiers, optional, and rest arguments.
	 *
	 * @param typeSpecifiers the required arguments
	 * @param optional       the optional arguments
	 * @param rest           the rest arguments
	 */
	public ValuesTypeSpecifier(final List<TypeSpecifier> typeSpecifiers, final Optional<TypeSpecifier> optional,
							   final Rest<TypeSpecifier> rest) {
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
		return new HashCodeBuilder()
				.append(typeSpecifiers)
				.append(optional)
				.append(rest)
				.toHashCode();
	}

	@Override
	public String toString() {
		return "ValuesTypeSpecifier{"
				+ "typeSpecifiers=" + typeSpecifiers
				+ ", optional=" + optional
				+ ", rest=" + rest
				+ '}';
	}
}
