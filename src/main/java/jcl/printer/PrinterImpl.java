/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer;

import java.util.Map;
import javax.annotation.Resource;

import jcl.LispStruct;
import jcl.printer.impl.LispPrinter;
import org.springframework.stereotype.Component;

@Component
public class PrinterImpl implements Printer {

	private static final long serialVersionUID = -3051919400352866531L;

	@Resource
	private Map<Class<? extends LispStruct>, LispPrinter<LispStruct>> structPrinterStrategies;

	@Override
	public String print(final LispStruct object) {

		final LispPrinter<LispStruct> printer = structPrinterStrategies.get(object.getClass());
		if (printer == null) {
			final String typeClassName = object.getType().getClass().getName().toUpperCase();
			return "#<" + typeClassName + '>';
		}

		return printer.print(object);
	}
}
