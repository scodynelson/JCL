package jcl.typespecifiers;

import jcl.LispType;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * An {@code OrTypeSpecifier} denotes the set of all objects of the type determined by the union of the typespecs.
 * The type specifiers (or) and nil are equivalent. The symbol or is not valid as a type specifier; and, specifically,
 * it is not an abbreviation for (or).
 */
public class OrTypeSpecifier implements CompoundTypeSpecifier {

	private final List<LispType> types;

	/**
	 * Constructs a new {@code OrTypeSpecifier} that matches the provided types by 'or' logic.
	 *
	 * @param types an array of {@code LispType}s
	 */
	public OrTypeSpecifier(final LispType... types) {
		this.types = new ArrayList<>(Arrays.asList(types));
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if (!(obj instanceof LispType)) {
			return false;
		}

		final LispType lispType = (LispType) obj;

		boolean result = false;
		for (final LispType type : types) {
			result = result || lispType.equals(type) || type.equals(lispType);
		}
		return result;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder()
				.append(types)
				.toHashCode();
	}

	@Override
	public String toString() {
		return "OrTypeSpecifier{"
				+ "types=" + types
				+ '}';
	}
}
