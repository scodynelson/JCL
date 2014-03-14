package jcl.typespecifiers.compound;

import jcl.LispType;
import jcl.typespecifiers.CompoundTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code NotTypeSpecifier} denotes the set of all objects that are not of the type typespec.
 * The symbol not is not valid as a type specifier.
 */
public class NotTypeSpecifier implements CompoundTypeSpecifier {

	private final LispType type;

	/**
	 * Constructs a new {@code NotTypeSpecifier} that matches what is not of the type argument provided.
	 *
	 * @param type a {@code LispType}
	 */
	public NotTypeSpecifier(final LispType type) {
		this.type = type;
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

		return !type.equals(lispType);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder()
				.append(type)
				.toHashCode();
	}

	@Override
	public String toString() {
		return "NotTypeSpecifier{"
				+ "type=" + type
				+ '}';
	}
}
