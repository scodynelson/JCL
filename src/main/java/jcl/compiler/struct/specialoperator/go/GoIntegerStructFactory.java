/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.go;

import jcl.numbers.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public class GoIntegerStructFactory extends GoStructFactory<IntegerStruct> {

	public GoIntegerStruct getGoElement(final IntegerStruct tag) {
		return new GoIntegerStruct(tag);
	}
}
