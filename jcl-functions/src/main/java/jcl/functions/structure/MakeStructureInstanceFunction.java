/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.structure;

import jcl.lang.LispStruct;
import jcl.lang.StructureClassStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.function.SystemBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class MakeStructureInstanceFunction extends SystemBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAKE-STRUCTURE-INSTANCE";
	private static final String STRUCTURE_SYMBOL_ARGUMENT = "STRUCTURE-SYMBOL";

	public MakeStructureInstanceFunction() {
		super("Makes a new structure-object instance of the structure-class assigned to the provided symbol with the provided arguments as slot values.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(STRUCTURE_SYMBOL_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStruct structSymbol = arguments.getRequiredArgument(STRUCTURE_SYMBOL_ARGUMENT, SymbolStruct.class);

		final StructureClassStruct structureClass = structSymbol.getStructureClass();
		if (structureClass == null) {
			throw new ProgramErrorException("Provided symbol '" + structSymbol + "' does not have a defined structure-class.");
		}

		return structureClass.newInstance();
	}
}