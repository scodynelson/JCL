package jcl.typespecifiers;

import jcl.LispStruct;
import jcl.structs.functions.PredicateFunctionStruct;
import jcl.typespecifiers.CompoundTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code SatisfiesTypeSpecifier} denotes the set of all objects that satisfy the predicate predicate-name, which must
 * be a symbol whose global function definition is a one-argument predicate. A name is required for predicate-name; lambda
 * expressions are not allowed. The argument is required. The symbol * can be the argument, but it denotes itself (the symbol *),
 * and does not represent an unspecified value. The symbol satisfies is not valid as a type specifier.
 */
public class SatisfiesTypeSpecifier implements CompoundTypeSpecifier {

	private final PredicateFunctionStruct<LispStruct> predicate;

	/**
	 * Public constructor.
	 *
	 * @param predicate the predicate function to test satisfaction
	 */
	public SatisfiesTypeSpecifier(final PredicateFunctionStruct<LispStruct> predicate) {
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
}
