package jcl.types.typespecifiers.compound;

import jcl.structs.functions.FunctionStruct;
import jcl.types.typespecifiers.CompoundTypeSpecifier;

public class SatisfiesTypeSpecifier implements CompoundTypeSpecifier {

	private final FunctionStruct function;

	public SatisfiesTypeSpecifier(final FunctionStruct function) {
		this.function = function;
	}
}
