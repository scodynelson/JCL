package jcl.lang;

import java.util.List;

import jcl.lang.classes.StructureClassStruct;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import org.apache.commons.lang3.tuple.Pair;

/**
 * The {@link StructureObjectStruct} is the object representation of a Lisp 'structure-object' type.
 */
public interface StructureObjectStruct extends LispStruct {

	StructureClassStruct getStructureClass();

	SymbolStruct getStructureSymbol();

	StructureObjectStruct getParentStructure();

	List<Pair<SymbolStruct, LispStruct>> getSlots();

	LispStruct getSlot(final SymbolStruct slotName);

	void setSlot(final SymbolStruct slotName, final LispStruct newSlotValue);

	static StructureObjectStruct makeStructureInstance(final SymbolStruct structSymbol) {
		final StructureClassStruct structureClass = StructureClassStruct.getStructureClass(structSymbol);
		return structureClass.newInstance();
	}

	static LispStruct getStructureSlotValue(final SymbolStruct structureClassSymbol,
	                                        final StructureObjectStruct structureInstance,
	                                        final SymbolStruct slotName) {
		final StructureClassStruct symbolStructureClass = StructureClassStruct.getStructureClass(structureClassSymbol);
		final StructureClassStruct instanceStructureClass = structureInstance.getStructureClass();
		final LispStruct instanceStructureType = instanceStructureClass.typeOf();

		final LispStruct symbolStructureType = symbolStructureClass.typeOf();
		if (!symbolStructureType.eq(instanceStructureType)) {
			throw new TypeErrorException(
					"Error: The value " + structureInstance + " is not of the expected type " + symbolStructureType + '.');
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

	static LispStruct setStructureSlotValue(final SymbolStruct structureClassSymbol,
	                                        final StructureObjectStruct structureInstance,
	                                        final SymbolStruct slotName,
	                                        final LispStruct slotValue) {
		final StructureClassStruct symbolStructureClass = StructureClassStruct.getStructureClass(structureClassSymbol);
		final StructureClassStruct instanceStructureClass = structureInstance.getStructureClass();
		final LispStruct instanceStructureType = instanceStructureClass.typeOf();

		final LispStruct symbolStructureType = symbolStructureClass.typeOf();
		if (!symbolStructureType.eq(instanceStructureType)) {
			throw new TypeErrorException(
					"Error: The value " + structureInstance + " is not of the expected type " + symbolStructureType + '.');
		}

		return innerSetStructureSlotValue(symbolStructureClass, structureInstance, slotName, slotValue);
	}

	private static LispStruct innerSetStructureSlotValue(final StructureClassStruct symbolStructureClass,
	                                                     final StructureObjectStruct structureInstance,
	                                                     final SymbolStruct slotName, final LispStruct slotValue) {

		final StructureClassStruct instanceStructureClass = structureInstance.getStructureClass();

		if (symbolStructureClass.eq(instanceStructureClass)) {
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
