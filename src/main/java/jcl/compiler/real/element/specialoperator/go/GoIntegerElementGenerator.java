/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator.go;

import jcl.numbers.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public class GoIntegerElementGenerator extends GoElementGenerator<IntegerStruct> {

	public GoIntegerElement generateGoElement(final IntegerStruct tag) {
		return new GoIntegerElement(tag);
	}
}
