package jcl.lang;

import java.util.List;

final class BitLogicContents {

	final List<IntegerStruct> contents1;
	final List<IntegerStruct> contents2;
	final int index;

	BitLogicContents(final List<IntegerStruct> contents1, final List<IntegerStruct> contents2,
	                 final int index) {
		this.contents1 = contents1;
		this.contents2 = contents2;
		this.index = index;
	}
}
