/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.struct.specialoperator.go.GoStruct;
import jcl.lang.condition.exception.ProgramErrorException;

/**
 * Exception used to facilitate the tagbody/go transfer-of-control (ToC) strategy.
 */
public class GoException extends ProgramErrorException {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 4353009450417754927L;

	/**
	 * The index of the {@link GoStruct} tag in the 'tagbody' TableSwitch generated via the {@link
	 * TagbodyCodeGenerator} that matches this exception.
	 */
	private final int tagIndex;

	/**
	 * Public constructor.
	 *
	 * @param tagIndex
	 * 		the index of the {@link GoStruct} tag in the 'tagbody' TableSwitch generated via the {@link
	 * 		TagbodyCodeGenerator}
	 */
	public GoException(final int tagIndex) {
		super("Tag Index: " + tagIndex);
		// TODO: should we store the tag on here???
		this.tagIndex = tagIndex;
	}

	/**
	 * Getter for the values of the {@link #tagIndex} property.
	 *
	 * @return the value of the {@link #tagIndex} property
	 */
	@SuppressWarnings({"unused", "SuppressionAnnotation"}) // NOTE: This is used inside the compiler bytecode generation
	public int getTagIndex() {
		return tagIndex;
	}
}
