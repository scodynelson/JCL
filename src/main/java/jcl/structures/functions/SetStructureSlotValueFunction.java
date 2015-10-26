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
public final class SetStructureSlotValueFunction extends FunctionStruct {

	public static final SymbolStruct<?> SET_STRUCTURE_SLOT_VALUE = GlobalPackageStruct.SYSTEM.intern("SET-STRUCTURE-SLOT-VALUE").getSymbol();

	private static final long serialVersionUID = -5380342029205088545L;

	private SetStructureSlotValueFunction() {
		super("Sets the slot value matching the provided symbol for the provided structure-object to the provided value.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		SET_STRUCTURE_SLOT_VALUE.setFunction(this);
		GlobalPackageStruct.SYSTEM.export(SET_STRUCTURE_SLOT_VALUE);
	}

	private static OrdinaryLambdaList getInitLambdaListBindings() {

		final List<RequiredParameter> requiredBindings = new ArrayList<>();

		final SymbolStruct<?> structureClassArgSymbol = GlobalPackageStruct.SYSTEM.intern("STRUCTURE-CLASS").getSymbol();
		final RequiredParameter structureClassArgRequiredBinding = new RequiredParameter(structureClassArgSymbol);
		requiredBindings.add(structureClassArgRequiredBinding);

		final SymbolStruct<?> structureInstanceArgSymbol = GlobalPackageStruct.SYSTEM.intern("STRUCTURE-INSTANCE").getSymbol();
		final RequiredParameter structureInstanceArgRequiredBinding = new RequiredParameter(structureInstanceArgSymbol);
		requiredBindings.add(structureInstanceArgRequiredBinding);

		final SymbolStruct<?> slotNameArgSymbol = GlobalPackageStruct.SYSTEM.intern("SLOT-NAME").getSymbol();
		final RequiredParameter slotNameArgRequiredBinding = new RequiredParameter(slotNameArgSymbol);
		requiredBindings.add(slotNameArgRequiredBinding);

		final SymbolStruct<?> slotValueArgSymbol = GlobalPackageStruct.SYSTEM.intern("SLOT-VALUE").getSymbol();
		final RequiredParameter slotValueArgRequiredBinding = new RequiredParameter(slotValueArgSymbol);
		requiredBindings.add(slotValueArgRequiredBinding);

		return new OrdinaryLambdaList.Builder().requiredBindings(requiredBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final SymbolStruct<?> structureClassSymbol = (SymbolStruct) lispStructs[0];
		final StructureObjectStruct structureInstance = (StructureObjectStruct) lispStructs[1];
		final SymbolStruct<?> slotName = (SymbolStruct) lispStructs[2];
		final LispStruct slotValue = lispStructs[3];
		return setStructureSlotValue(structureClassSymbol, structureInstance, slotName, slotValue);
	}

	public LispStruct setStructureSlotValue(final SymbolStruct<?> structureClassSymbol, final StructureObjectStruct structureInstance,
	                                        final SymbolStruct<?> slotName, final LispStruct slotValue) {

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

		return innerSetStructureSlotValue(symbolStructureClass, structureInstance, slotName, slotValue);
	}

	private LispStruct innerSetStructureSlotValue(final StructureClassStruct symbolStructureClass,
	                                              final StructureObjectStruct structureInstance,
	                                              final SymbolStruct<?> slotName, final LispStruct slotValue) {

		final StructureClassStruct instanceStructureClass = structureInstance.getStructureClass();

		if (symbolStructureClass.equals(instanceStructureClass)) {
			structureInstance.setSlot(slotName, slotValue);
			return slotValue;
		}

		final StructureObjectStruct parentStructure = structureInstance.getParentStructure();
		if (parentStructure == null) {
			throw new SimpleErrorException("Slot " + slotName + " is not present for structure " + structureInstance);
		}

		return innerSetStructureSlotValue(symbolStructureClass, parentStructure, slotName, slotValue);
	}
}
