/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.element;

import jcl.compiler.real.element.CommentElement;
import jcl.printer.impl.CommentPrinter;
import org.springframework.stereotype.Component;

@Component
public class CommentElementPrinter extends CommentPrinter<CommentElement> {

	@Override
	protected String getCommentString(final CommentElement object) {
		return object.getCommentString();
	}
}
