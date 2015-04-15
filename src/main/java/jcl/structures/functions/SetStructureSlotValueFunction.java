/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.structures.functions;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
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

	public static final SymbolStruct<?> GET_STRUCTURE_SLOT_VALUE = new SymbolStruct<>("GET-STRUCTURE-SLOT-VALUE", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = -5380342029205088545L;

	private SetStructureSlotValueFunction() {
		super("Gets the slot value matching the provided symbol for the provided structure-object.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		GET_STRUCTURE_SLOT_VALUE.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final List<RequiredBinding> requiredBindings = new ArrayList<>();

		final SymbolStruct<?> structureClassArgSymbol = new SymbolStruct<>("STRUCTURE-CLASS", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation structureClassArgAllocation = new ParameterAllocation(0);
		final RequiredBinding structureClassArgRequiredBinding = new RequiredBinding(structureClassArgSymbol, structureClassArgAllocation);
		requiredBindings.add(structureClassArgRequiredBinding);

		final SymbolStruct<?> structureInstanceArgSymbol = new SymbolStruct<>("STRUCTURE-INSTANCE", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation structureInstanceArgAllocation = new ParameterAllocation(1);
		final RequiredBinding structureInstanceArgRequiredBinding = new RequiredBinding(structureInstanceArgSymbol, structureInstanceArgAllocation);
		requiredBindings.add(structureInstanceArgRequiredBinding);

		final SymbolStruct<?> slotNameArgSymbol = new SymbolStruct<>("SLOT-NAME", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation slotNameArgAllocation = new ParameterAllocation(2);
		final RequiredBinding slotNameArgRequiredBinding = new RequiredBinding(slotNameArgSymbol, slotNameArgAllocation);
		requiredBindings.add(slotNameArgRequiredBinding);

		final SymbolStruct<?> slotValueArgSymbol = new SymbolStruct<>("SLOT-VALUE", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation slotValueArgAllocation = new ParameterAllocation(3);
		final RequiredBinding slotValueArgRequiredBinding = new RequiredBinding(slotValueArgSymbol, slotValueArgAllocation);
		requiredBindings.add(slotValueArgRequiredBinding);

		final List<OptionalBinding> optionalBindings = Collections.emptyList();

		final RestBinding restBinding = null;

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
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
