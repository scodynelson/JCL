/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.struct;

import jcl.LispStruct;
import jcl.lists.ConsStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.printer.impl.ConsPrinter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ConsStructPrinter extends ConsPrinter<ConsStruct> {

	private static final long serialVersionUID = 2018276801770003739L;

	@Autowired
	private Printer printer;

	@Override
	protected boolean isCircular(final ConsStruct object) {
		return object.isCircular();
	}

	/**
	 * Prints the inner cons value of the provided {@code consStruct}, handling the recursive nature of printing each
	 * internal layer repeatedly until the last cdr has been identified as either being a {@link NullStruct} or not a
	 * ConsStruct.
	 *
	 * @param object
	 * 		the ConsStruct to print each inner cons
	 */
	@Override
	protected void printElements(final ConsStruct object, final StringBuilder stringBuilder) {

		final LispStruct car = object.getCar();
		final String printedCar = printer.print(car);

		stringBuilder.append(printedCar);

		if (object.getCdr() instanceof ConsStruct) {
			final ConsStruct cdrAsCons = (ConsStruct) object.getCdr();
			final String innerConsPrinted = printer.print(cdrAsCons);

			stringBuilder.append(' ');
			stringBuilder.append(innerConsPrinted);
		} else if (!object.getCdr().equals(NullStruct.INSTANCE)) {
			stringBuilder.append(" . ");

			final LispStruct cdr = object.getCdr();
			final String printedCdr = printer.print(cdr);

			stringBuilder.append(printedCdr);
		}
	}
}
