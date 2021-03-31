/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.io.Serial;

import jcl.compiler.struct.specialoperator.go.GoStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import lombok.Getter;

/**
 * Exception used to facilitate the tagbody/go transfer-of-control (ToC) strategy.
 */
@Getter
public class GoException extends ProgramErrorException {

	/**
	 * Serializable Version Unique Identifier.
	 */
	@Serial
	private static final long serialVersionUID = 1L;

	/**
	 * The index of the {@link GoStruct} tag in the 'tagbody' TableSwitch generated that matches this exception.
	 */
	private final int tagIndex;

	/**
	 * Public constructor.
	 *
	 * @param tagIndex
	 * 		the index of the {@link GoStruct} tag in the 'tagbody' TableSwitch generated
	 */
	public GoException(final int tagIndex) {
		super("Tag Index: " + tagIndex);
		// TODO: should we store the tag on here???
		this.tagIndex = tagIndex;
	}
}
