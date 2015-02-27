/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator.go;

import jcl.compiler.real.element.IntegerElement;
import org.springframework.stereotype.Component;

@Component
public class GoIntegerElementGenerator extends GoElementGenerator<IntegerElement> {

	public GoIntegerElement generateGoElement(final IntegerElement tag) {
		return new GoIntegerElement(tag);
	}
}
