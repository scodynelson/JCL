package jcl.types.typespecifiers;

import jcl.LispType;
import jcl.types.TypeBaseClass;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * An {@link OrTypeSpecifier} denotes the set of all objects of the type determined by the union of the typespecs.
 * The type specifiers (or) and nil are equivalent. The symbol or is not valid as a type specifier; and, specifically,
 * it is not an abbreviation for (or).
 */
public class OrTypeSpecifier extends TypeBaseClass implements CompoundTypeSpecifier {

	private final List<LispType> types;

	/**
	 * Constructs a new OrTypeSpecifier that matches the provided types by 'or' logic.
	 *
	 * @param types
	 * 		an array of {@link LispType}s
	 */
	public OrTypeSpecifier(final LispType... types) {
		this("T", types); // TODO: Should this be 'T'???
	}

	/**
	 * Constructs a new OrTypeSpecifier that matches the provided types by 'or' logic.
	 *
	 * @param name
	 * 		the name of the symbol type
	 * @param types
	 * 		an array of {@link LispType}s
	 */
	protected OrTypeSpecifier(final String name, final LispType... types) {
		super(name);
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
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
