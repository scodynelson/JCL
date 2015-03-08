/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The object representation of a Lisp 'comment' type.
 */
public class CommentStruct implements LispStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 8261638201983129004L;

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
	public CommentStruct(final String commentString) {
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
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
