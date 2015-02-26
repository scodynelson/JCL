/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.element;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.printer.Printer;
import jcl.printer.impl.ConsPrinter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Iterator;
import java.util.List;

@Component
public class ConsElementPrinter extends ConsPrinter<ConsElement> {

	@Autowired
	private Printer printer;

	@Override
	protected boolean isCircular(final ConsElement object) {
		return false;
	}

	@Override
	protected void printElements(final ConsElement object, final StringBuilder stringBuilder) {

		final List<SimpleElement> elements = object.getElements();

		final boolean isDotted = object.isDotted();

		final Iterator<SimpleElement> elementIterator = elements.iterator();

		while (elementIterator.hasNext()) {

			final SimpleElement nextElement = elementIterator.next();
			final String innerConsPrinted = printer.print(nextElement);

			if (!elementIterator.hasNext() && isDotted) {
				stringBuilder.append(". ");
			}

			stringBuilder.append(innerConsPrinted);

			if (elementIterator.hasNext()) {
				stringBuilder.append(' ');
			}
		}
	}
}
