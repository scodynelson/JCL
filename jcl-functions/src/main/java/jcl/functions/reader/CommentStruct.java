/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.LispStruct;

/**
 * The object representation of a Lisp 'comment' type.
 */
public class CommentStruct implements LispStruct {

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
//		return object.getCommentString();
		// NOTE: disabling comment printing for now.
		return "";
	}
}
