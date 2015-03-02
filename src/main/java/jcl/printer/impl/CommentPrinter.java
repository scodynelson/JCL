/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

public abstract class CommentPrinter<O> implements LispPrinter<O> {

	private static final long serialVersionUID = -6066380287335603328L;

	@Override
	public String print(final O object) {
		return getCommentString(object);
	}

	protected abstract String getCommentString(O object);
}
