package jcl.typespecifiers;

import jcl.LispStruct;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.types.TypeBaseClass;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * A {@link MemberTypeSpecifier} denotes the set containing the named objects. An object is of this type if and only if
 * it is eql to one of the specified objects. The type specifiers (member) and nil are equivalent. * can be among the
 * objects, but if so it denotes itself (the symbol *) and does not represent an unspecified value. The symbol member is
 * not valid as a type specifier; and, specifically, it is not an abbreviation for either (member) or (member *).
 */
public class MemberTypeSpecifier extends TypeBaseClass implements CompoundTypeSpecifier {

	private final List<LispStruct> lispStructs;

	/**
	 * Public constructor.
	 *
	 * @param lispStructs the lisp structures that define membership equality
	 */
	public MemberTypeSpecifier(final LispStruct... lispStructs) {
		this("T", lispStructs); // TODO: Should this be 'T'???
	}

	/**
	 * Protected constructor.
	 *
	 * @param name        the name of the symbol type
	 * @param lispStructs the lisp structures that define membership equality
	 */
	protected MemberTypeSpecifier(final String name, final LispStruct... lispStructs) {
		super(name, GlobalPackageStruct.COMMON_LISP);
		this.lispStructs = new ArrayList<>(Arrays.asList(lispStructs));
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if (!(obj instanceof LispStruct)) {
			return false;
		}

		// TODO: does this account for x.equals(y) and y.equals(x)???
		final LispStruct lispStruct = (LispStruct) obj;
		for (final LispStruct memberLispStruct : lispStructs) {
			if (lispStruct.equals(memberLispStruct)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder()
				.append(lispStructs)
				.toHashCode();
	}

	@Override
	public String toString() {
		return "MemberTypeSpecifier{"
				+ "lispStructs=" + lispStructs
				+ '}';
	}
}
