/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.LispStruct;
import jcl.reader.macrofunction.CommentStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class CommentElement implements SimpleElement {

	private static final long serialVersionUID = -3063330797183559711L;

	/**
	 * The read in comment string.
	 */
	private final String commentString;

	/**
	 * Public constructor.
	 *
	 * @param commentString
	 * 		the {@link String} comment value
	 */
	public CommentElement(final String commentString) {
		this.commentString = commentString;
	}

	/**
	 * Getter for character {@link #commentString} property.
	 *
	 * @return character {@link #commentString} property
	 */
	public String getCommentString() {
		return commentString;
	}

	@Override
	public LispStruct toLispStruct() {
		return new CommentStruct(commentString);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
