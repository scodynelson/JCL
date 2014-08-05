package jcl.typespecifiers;

import jcl.LispType;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.types.TypeBaseClass;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link NotTypeSpecifier} denotes the set of all objects that are not of the type typespec.
 * The symbol not is not valid as a type specifier.
 */
public class NotTypeSpecifier extends TypeBaseClass implements CompoundTypeSpecifier {

	private final LispType type;

	/**
	 * Constructs a new NotTypeSpecifier that matches what is not of the type argument provided.
	 *
	 * @param type a {@link LispType}
	 */
	public NotTypeSpecifier(final LispType type) {
		this("T", type); // TODO: Should this be 'T'???
	}

	/**
	 * Constructs a new NotTypeSpecifier that matches what is not of the type argument provided.
	 *
	 * @param name the name of the symbol type
	 * @param type a {@link LispType}
	 */
	protected NotTypeSpecifier(final String name, final LispType type) {
		super(name, GlobalPackageStruct.COMMON_LISP);
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

		return !lispType.equals(type) && !type.equals(lispType);
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
