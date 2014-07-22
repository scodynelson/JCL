package jcl.printer;

import jcl.numbers.IntegerStruct;

public class IntegerStructPrinter {

	public static String print(final IntegerStruct integerStruct) {
		return integerStruct.getBigInteger().toString();
	}
}
