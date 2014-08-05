package jcl.typespecifiers;

import jcl.LispStruct;
import jcl.structs.functions.PredicateFunctionStruct;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.types.TypeBaseClass;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link SatisfiesTypeSpecifier} denotes the set of all objects that satisfy the predicate predicate-name, which must
 * be a symbol whose global function definition is a one-argument predicate. A name is required for predicate-name; lambda
 * expressions are not allowed. The argument is required. The symbol * can be the argument, but it denotes itself (the symbol *),
 * and does not represent an unspecified value. The symbol satisfies is not valid as a type specifier.
 */
public class SatisfiesTypeSpecifier extends TypeBaseClass implements CompoundTypeSpecifier {

	private final PredicateFunctionStruct<LispStruct> predicate;

	/**
	 * Public constructor.
	 *
	 * @param predicate the predicate function to test satisfaction
	 */
	public SatisfiesTypeSpecifier(final PredicateFunctionStruct<LispStruct> predicate) {
		this("T", predicate); // TODO: Should this be 'T'???
	}

	/**
	 * Protected constructor.
	 *
	 * @param name      the name of the symbol type
	 * @param predicate the predicate function to test satisfaction
	 */
	protected SatisfiesTypeSpecifier(final String name, final PredicateFunctionStruct<LispStruct> predicate) {
		super(name, GlobalPackageStruct.COMMON_LISP);
		this.predicate = predicate;
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if (!(obj instanceof LispStruct)) {
			return false;
		}

		final LispStruct lispStruct = (LispStruct) obj;

		return predicate.evaluate(lispStruct);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder()
				.append(predicate)
				.toHashCode();
	}

	@Override
	public String toString() {
		return "SatisfiesTypeSpecifier{"
				+ "predicate=" + predicate
				+ '}';
	}
}
