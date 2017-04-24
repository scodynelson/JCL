/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type.typespecifier;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import jcl.type.LispType;
import jcl.type.TypeBaseClass;

/**
 * An {@link AndTypeSpecifier} denotes the set of all objects of the type determined by the intersection of the
 * typespecs. The type specifiers (and) and t are equivalent. The symbol and is not valid as a type specifier, and,
 * specifically, it is not an abbreviation for (and).
 */
public class AndTypeSpecifier extends TypeBaseClass implements CompoundTypeSpecifier {

	/**
	 * The list of {@link LispType}s to 'AND'.
	 */
	private final List<LispType> types;

	/**
	 * Constructs a new AndTypeSpecifier that matches the provided types by 'and' logic.
	 *
	 * @param types
	 * 		an array of {@link LispType}s
	 */
	public AndTypeSpecifier(final LispType... types) {
		this("T", types); // TODO: Should this be 'T'???
	}

	/**
	 * Constructs a new AndTypeSpecifier that matches the provided types by 'and' logic with the symbol-name
	 * {@code typeName}.
	 *
	 * @param name
	 * 		the name of the symbol type
	 * @param types
	 * 		an array of {@link LispType}s
	 */
	protected AndTypeSpecifier(final String name, final LispType... types) {
		super(name);
		this.types = new ArrayList<>(Arrays.asList(types));
	}

	@Override
	public boolean typeEquals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if (!(obj instanceof LispType)) {
			return false;
		}

		final LispType lispType = (LispType) obj;

		boolean result = true;
		for (final LispType type : types) {
			result = result && lispType.isOfType(type);
		}
		return result;
	}
}
