package jcl.typespecifiers.compound;

import jcl.structs.FunctionStruct;
import jcl.typespecifiers.CompoundTypeSpecifier;

public class SatisfiesTypeSpecifier implements CompoundTypeSpecifier {

	private final FunctionStruct function;

	public SatisfiesTypeSpecifier(final FunctionStruct function) {
		this.function = function;
	}
}
