/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.struct;

import jcl.printer.impl.CommentPrinter;
import jcl.reader.macrofunction.CommentStruct;
import org.springframework.stereotype.Component;

@Component
public class CommentStructPrinter extends CommentPrinter<CommentStruct> {

	@Override
	protected String getCommentString(final CommentStruct object) {
		return object.getCommentString();
	}
}
