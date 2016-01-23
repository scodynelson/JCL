/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.printer.LispPrinter;
import jcl.reader.macrofunction.CommentStruct;
import org.springframework.stereotype.Component;

@Component
public class CommentStructPrinter implements LispPrinter<CommentStruct> {

	@Override
	public String print(final CommentStruct object) {
//		return object.getCommentString();
		// NOTE: disabling comment printing for now.
		return "";
	}
}
