/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.go;

import jcl.numbers.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public class GoIntegerStructFactory extends GoStructFactory<IntegerStruct> {

	private static final long serialVersionUID = -4098304819342554338L;

	public GoIntegerStruct getGoElement(final IntegerStruct tag) {
		return new GoIntegerStruct(tag);
	}
}
