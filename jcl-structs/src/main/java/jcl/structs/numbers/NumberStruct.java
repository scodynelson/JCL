package jcl.structs.numbers;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.numbers.Number;

/**
 * The {@code NumberStruct} is the object representation of a Lisp 'number' type.
 */
public class NumberStruct implements LispStruct {

	@Override
	public LispType getType() {
		return Number.INSTANCE;
	}
}
