package jcl.lang;

import java.util.List;

import jcl.lang.classes.StructureClassStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.type.LispType;
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
		final StructureClassStruct structureClass = structSymbol.getStructureClass();
		if (structureClass == null) {
			throw new ProgramErrorException(
					"Provided symbol '" + structSymbol + "' does not have a defined structure-class.");
		}

		return structureClass.newInstance();
	}

	static LispStruct getStructureSlotValue(final SymbolStruct structureClassSymbol,
	                                        final StructureObjectStruct structureInstance,
	                                        final SymbolStruct slotName) {
		final StructureClassStruct symbolStructureClass = structureClassSymbol.getStructureClass();
		if (symbolStructureClass == null) {
			throw new ProgramErrorException(
					"Provided symbol '" + structureClassSymbol + "' does not have a defined structure-class.");
		}

		final StructureClassStruct instanceStructureClass = structureInstance.getStructureClass();
		final LispType instanceStructureType = instanceStructureClass.getType();

		final LispType symbolStructureType = symbolStructureClass.getType();
		if (symbolStructureType.isNotOfType(instanceStructureType)) {
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
		final StructureClassStruct symbolStructureClass = structureClassSymbol.getStructureClass();
		if (symbolStructureClass == null) {
			throw new ProgramErrorException(
					"Provided symbol '" + structureClassSymbol + "' does not have a defined structure-class.");
		}

		final StructureClassStruct instanceStructureClass = structureInstance.getStructureClass();
		final LispType instanceStructureType = instanceStructureClass.getType();

		final LispType symbolStructureType = symbolStructureClass.getType();
		if (symbolStructureType.isNotOfType(instanceStructureType)) {
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
