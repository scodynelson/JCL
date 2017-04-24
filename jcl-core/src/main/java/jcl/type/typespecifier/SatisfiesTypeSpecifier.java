/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type.typespecifier;

import jcl.lang.BooleanStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.type.TypeBaseClass;

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
	 * The {@link FunctionStruct} to check against the 'SATISFIES' type specifier.
	 */
	private final FunctionStruct predicate;

	/**
	 * Public constructor.
	 *
	 * @param predicate
	 * 		the predicate function to test satisfaction
	 */
	public SatisfiesTypeSpecifier(final FunctionStruct predicate) {
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
	protected SatisfiesTypeSpecifier(final String name, final FunctionStruct predicate) {
		super(name);
		this.predicate = predicate;
	}

	@Override
	public boolean typeEquals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if (!(obj instanceof LispStruct)) {
			return false;
		}

		final LispStruct lispStruct = (LispStruct) obj;

		// TODO: this probably isn't very safe...
		return ((BooleanStruct) predicate.apply(lispStruct)).booleanValue();
	}
}
