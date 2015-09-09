/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.conditions.exceptions.ProgramErrorException;

public class GoException extends ProgramErrorException {

	private static final long serialVersionUID = 4353009450417754927L;

	private final int tagIndex;

	public GoException(final int tagIndex) {
		super("Tag Index: " + tagIndex);
		// TODO: should we store the tag on here???
		this.tagIndex = tagIndex;
	}

	@SuppressWarnings({"unused", "SuppressionAnnotation"}) // NOTE: This is used inside the compiler bytecode generation
	public int getTagIndex() {
		return tagIndex;
	}
}
