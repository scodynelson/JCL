/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.lang.LispStruct;
import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * The object representation of a Lisp 'comment' type.
 */
@Getter
@AllArgsConstructor
public class CommentStruct implements LispStruct {

	/**
	 * The read in comment string.
	 */
	private final String commentString;

	@Override
	public String toString() {
		return commentString;
	}
}
