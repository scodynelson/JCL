/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type.typespecifier;

import jcl.type.LispType;
import jcl.type.TypeBaseClass;

/**
 * A {@link NotTypeSpecifier} denotes the set of all objects that are not of the type typespec.
 * The symbol not is not valid as a type specifier.
 */
public class NotTypeSpecifier extends TypeBaseClass implements CompoundTypeSpecifier {

	/**
	 * The {@link LispType} to 'NOT'.
	 */
	private final LispType type;

	/**
	 * Constructs a new NotTypeSpecifier that matches what is not of the type argument provided.
	 *
	 * @param type
	 * 		a {@link LispType}
	 */
	public NotTypeSpecifier(final LispType type) {
		this("T", type); // TODO: Should this be 'T'???
	}

	/**
	 * Constructs a new NotTypeSpecifier that matches what is not of the type argument provided.
	 *
	 * @param name
	 * 		the name of the symbol type
	 * @param type
	 * 		a {@link LispType}
	 */
	protected NotTypeSpecifier(final String name, final LispType type) {
		super(name);
		this.type = type;
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

		return lispType.isNotOfType(type);
	}
}
