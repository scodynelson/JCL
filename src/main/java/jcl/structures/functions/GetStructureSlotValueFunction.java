/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.structures.functions;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.structures.StructureClassStruct;
import jcl.structures.StructureObjectStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public final class GetStructureSlotValueFunction extends FunctionStruct {

	public static final SymbolStruct GET_STRUCTURE_SLOT_VALUE = GlobalPackageStruct.SYSTEM.intern("GET-STRUCTURE-SLOT-VALUE").getSymbol();

	private static final long serialVersionUID = 8765173339549970564L;

	private GetStructureSlotValueFunction() {
		super("Gets the slot value matching the provided symbol for the provided structure-object.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		GET_STRUCTURE_SLOT_VALUE.setFunction(this);
		GlobalPackageStruct.SYSTEM.export(GET_STRUCTURE_SLOT_VALUE);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final List<RequiredParameter> requiredBindings = new ArrayList<>();

		final SymbolStruct structureClassArgSymbol = GlobalPackageStruct.SYSTEM.intern("STRUCTURE-CLASS").getSymbol();
		final RequiredParameter structureClassArgRequiredBinding = new RequiredParameter(structureClassArgSymbol);
		requiredBindings.add(structureClassArgRequiredBinding);

		final SymbolStruct structureInstanceArgSymbol = GlobalPackageStruct.SYSTEM.intern("STRUCTURE-INSTANCE").getSymbol();
		final RequiredParameter structureInstanceArgRequiredBinding = new RequiredParameter(structureInstanceArgSymbol);
		requiredBindings.add(structureInstanceArgRequiredBinding);

		final SymbolStruct slotNameArgSymbol = GlobalPackageStruct.SYSTEM.intern("SLOT-NAME").getSymbol();
		final RequiredParameter slotNameArgRequiredBinding = new RequiredParameter(slotNameArgSymbol);
		requiredBindings.add(slotNameArgRequiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                       .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final SymbolStruct structureClassSymbol = (SymbolStruct) lispStructs[0];
		final StructureObjectStruct structureInstance = (StructureObjectStruct) lispStructs[1];
		final SymbolStruct slotName = (SymbolStruct) lispStructs[2];
		return getStructureSlotValue(structureClassSymbol, structureInstance, slotName);
	}

	public LispStruct getStructureSlotValue(final SymbolStruct structureClassSymbol, final StructureObjectStruct structureInstance,
	                                        final SymbolStruct slotName) {

		final StructureClassStruct symbolStructureClass = structureClassSymbol.getStructureClass();
		if (symbolStructureClass == null) {
			throw new ProgramErrorException("Provided symbol '" + structureClassSymbol + "' does not have a defined structure-class.");
		}

		final StructureClassStruct instanceStructureClass = structureInstance.getStructureClass();
		final LispType instanceStructureType = instanceStructureClass.getType();

		final LispType symbolStructureType = symbolStructureClass.getType();
		if (!symbolStructureType.equals(instanceStructureType)) {
			throw new TypeErrorException("Error: The value " + structureInstance + " is not of the expected type " + symbolStructureType + '.');
		}

		return innerGetStructureSlotValue(symbolStructureClass, structureInstance, slotName);
	}

	private LispStruct innerGetStructureSlotValue(final StructureClassStruct symbolStructureClass,
	                                              final StructureObjectStruct structureInstance,
	                                              final SymbolStruct slotName) {

		final StructureClassStruct instanceStructureClass = structureInstance.getStructureClass();

		if (symbolStructureClass.equals(instanceStructureClass)) {
			return structureInstance.getSlot(slotName);
		}

		final StructureObjectStruct parentStructure = structureInstance.getParentStructure();
		if (parentStructure == null) {
			throw new SimpleErrorException("Slot " + slotName + " is not present for structure " + structureInstance);
		}

		return innerGetStructureSlotValue(symbolStructureClass, parentStructure, slotName);
	}
}
