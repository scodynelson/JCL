/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type.typespecifier;

import jcl.lang.LispStruct;
import jcl.lang.function.PredicateFunctionStructBase;
import jcl.type.TypeBaseClass;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * A {@link SatisfiesTypeSpecifier} denotes the set of all objects that satisfy the predicate predicate-name, which
 * must be a symbol whose global function definition is a one-argument predicate. A name is required for
 * predicate-name; lambda expressions are not allowed. The argument is required. The symbol * can be the argument, but
 * it denotes itself (the symbol *), and does not represent an unspecified value. The symbol satisfies is not valid as
 * a
 * type specifier.
 */
public class SatisfiesTypeSpecifier extends TypeBaseClass implements CompoundTypeSpecifier {

	/**
	 * The {@link PredicateFunctionStructBase} to check against the 'SATISFIES' type specifier.
	 */
	private final PredicateFunctionStructBase<LispStruct> predicate;

	/**
	 * Public constructor.
	 *
	 * @param predicate
	 * 		the predicate function to test satisfaction
	 */
	public SatisfiesTypeSpecifier(final PredicateFunctionStructBase<LispStruct> predicate) {
		this("T", predicate); // TODO: Should this be 'T'???
	}

	/**
	 * Protected constructor.
	 *
	 * @param name
	 * 		the name of the symbol type
	 * @param predicate
	 * 		the predicate function to test satisfaction
	 */
	protected SatisfiesTypeSpecifier(final String name, final PredicateFunctionStructBase<LispStruct> predicate) {
		super(name);
		this.predicate = predicate;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(predicate)
		                            .toHashCode();
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
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(predicate)
		                                                                .toString();
	}
}
