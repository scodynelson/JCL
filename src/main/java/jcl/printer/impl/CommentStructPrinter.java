/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.printer.LispPrinter;
import jcl.reader.macrofunction.CommentStruct;
import org.springframework.stereotype.Component;

@Component
public class CommentStructPrinter implements LispPrinter<CommentStruct> {

	private static final long serialVersionUID = 3702811009619046535L;

	@Override
	public String print(final CommentStruct object) {
//		return object.getCommentString();
		// NOTE: disabling comment printing for now.
		return "";
	}
}
