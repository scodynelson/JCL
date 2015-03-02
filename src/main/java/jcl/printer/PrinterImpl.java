/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer;

import jcl.LispStruct;
import jcl.compiler.real.element.Element;
import jcl.printer.impl.LispPrinter;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Map;

@Component
public class PrinterImpl implements Printer {

	private static final long serialVersionUID = -3051919400352866531L;

	@Resource
	private Map<Class<? extends LispStruct>, LispPrinter<LispStruct>> structPrinterStrategies;

	@Resource
	private Map<Class<? extends Element>, LispPrinter<Element>> elementPrinterStrategies;

	@Override
	public String print(final LispStruct object) {

		final LispPrinter<LispStruct> printer = structPrinterStrategies.get(object.getClass());
		if (printer == null) {
			final String typeClassName = object.getType().getClass().getName().toUpperCase();
			return "#<" + typeClassName + '>';
		}

		return printer.print(object);
	}

	@Override
	public String print(final Element object) {

		final LispPrinter<Element> printer = elementPrinterStrategies.get(object.getClass());
		if (printer == null) {
			final String elementClass = object.getClass().getName().toUpperCase();
			throw new RuntimeException("Printer could not be found for element class: " + elementClass);
		}

		return printer.print(object);
	}
}
