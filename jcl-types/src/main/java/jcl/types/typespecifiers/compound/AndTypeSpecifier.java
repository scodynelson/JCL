package jcl.types.typespecifiers.compound;

import jcl.types.LispType;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * An {@code AndTypeSpecifier} denotes the set of all objects of the type determined by the intersection of the typespecs.
 * The type specifiers (and) and t are equivalent. The symbol and is not valid as a type specifier, and, specifically,
 * it is not an abbreviation for (and).
 */
public class AndTypeSpecifier implements CompoundTypeSpecifier {

	private final List<LispType> types;

	/**
	 * Constructs a new {@code AndTypeSpecifier} that matches the provided types by 'and' logic.
	 *
	 * @param types an array of {@code LispType}s
	 */
	public AndTypeSpecifier(final LispType... types) {
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

		boolean result = true;
		for (final LispType type : types) {
			result = result && lispType.equals(type);
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
		return "AndTypeSpecifier{"
				+ "types=" + types
				+ '}';
	}
}
