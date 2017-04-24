/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.structure;

import jcl.functions.SystemBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.StructureObjectStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.classes.StructureClassStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.type.LispType;
import org.springframework.stereotype.Component;

@Component
public final class GetStructureSlotValueFunction extends SystemBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "GET-STRUCTURE-SLOT-VALUE";
	private static final String STRUCTURE_CLASS_ARGUMENT = "STRUCTURE-CLASS";
	private static final String STRUCTURE_INSTANCE_ARGUMENT = "STRUCTURE-INSTANCE";
	private static final String SLOT_NAME_ARGUMENT = "SLOT-NAME";

	public GetStructureSlotValueFunction() {
		super("Gets the slot value matching the provided symbol for the provided structure-object.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(STRUCTURE_CLASS_ARGUMENT)
		                .requiredParameter(STRUCTURE_INSTANCE_ARGUMENT)
		                .requiredParameter(SLOT_NAME_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SymbolStruct structureClassSymbol = arguments.getRequiredArgument(STRUCTURE_CLASS_ARGUMENT, SymbolStruct.class);
		final StructureObjectStruct structureInstance = arguments.getRequiredArgument(STRUCTURE_INSTANCE_ARGUMENT, StructureObjectStruct.class);
		final SymbolStruct slotName = arguments.getRequiredArgument(SLOT_NAME_ARGUMENT, SymbolStruct.class);

		final StructureClassStruct symbolStructureClass = structureClassSymbol.getStructureClass();
		if (symbolStructureClass == null) {
			throw new ProgramErrorException("Provided symbol '" + structureClassSymbol + "' does not have a defined structure-class.");
		}

		final StructureClassStruct instanceStructureClass = structureInstance.getStructureClass();
		final LispType instanceStructureType = instanceStructureClass.getType();

		final LispType symbolStructureType = symbolStructureClass.getType();
		if (symbolStructureType.isNotOfType(instanceStructureType)) {
			throw new TypeErrorException("Error: The value " + structureInstance + " is not of the expected type " + symbolStructureType + '.');
		}

		return innerGetStructureSlotValue(symbolStructureClass, structureInstance, slotName);
	}

	private static LispStruct innerGetStructureSlotValue(final StructureClassStruct symbolStructureClass,
	                                                     final StructureObjectStruct structureInstance,
	                                                     final SymbolStruct slotName) {

		final StructureClassStruct instanceStructureClass = structureInstance.getStructureClass();

		if (symbolStructureClass.eq(instanceStructureClass)) {
			return structureInstance.getSlot(slotName);
		}

		final StructureObjectStruct parentStructure = structureInstance.getParentStructure();
		if (parentStructure == null) {
			throw new SimpleErrorException("Slot " + slotName + " is not present for structure " + structureInstance);
		}

		return innerGetStructureSlotValue(symbolStructureClass, parentStructure, slotName);
	}
}
