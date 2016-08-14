package jcl.lang;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import jcl.lang.condition.exception.SimpleErrorException;
import jcl.type.LispType;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

/**
 * The {@link StructureObjectStructImpl} is the object representation of a Lisp 'structure-object' type.
 */
public class StructureObjectStructImpl implements StructureObjectStruct {

	protected final StructureClassStruct structureClass;

	protected final SymbolStruct structureSymbol;

	protected final StructureObjectStruct parentStructure;

	protected final Map<SymbolStruct, LispStruct> slots = new LinkedHashMap<>();

	protected StructureObjectStructImpl(final StructureClassStruct structureClass, final SymbolStruct structureSymbol,
	                                    final StructureObjectStruct parentStructure) {
		this.structureClass = structureClass;
		this.structureSymbol = structureSymbol;
		this.parentStructure = parentStructure;
	}

	public StructureClassStruct getStructureClass() {
		return structureClass;
	}

	public SymbolStruct getStructureSymbol() {
		return structureSymbol;
	}

	public StructureObjectStruct getParentStructure() {
		return parentStructure;
	}

	public List<Pair<SymbolStruct, LispStruct>> getSlots() {
		final List<Pair<SymbolStruct, LispStruct>> allSlots = new ArrayList<>();

		if (parentStructure != null) {
			final List<Pair<SymbolStruct, LispStruct>> parentAllSlots = parentStructure.getSlots();
			allSlots.addAll(parentAllSlots);
		}

		for (final Map.Entry<SymbolStruct, LispStruct> slot : slots.entrySet()) {
			final SymbolStruct slotSymbol = slot.getKey();
			final LispStruct slotValue = slot.getValue();
			final Pair<SymbolStruct, LispStruct> pair = ImmutablePair.of(slotSymbol, slotValue);
			allSlots.add(pair);
		}

		return allSlots;
	}

	public LispStruct getSlot(final SymbolStruct slotName) {
		if (slots.containsKey(slotName)) {
			return slots.get(slotName);
		}

		throw new SimpleErrorException("Slot " + slotName + " is not present for structure " + this);
	}

	public void setSlot(final SymbolStruct slotName, final LispStruct newSlotValue) {
		if (slots.containsKey(slotName)) {
			slots.put(slotName, newSlotValue);
			return;
		}

		throw new SimpleErrorException("Slot " + slotName + " is not present for structure " + this);
	}

	@Override
	public LispType getType() {
		return structureClass.getType();
	}

	@Override
	public String toString() {
		final StringBuilder stringBuilder = new StringBuilder();

		stringBuilder.append("#S(");

		final String printedStructureSymbol = structureSymbol.toString();
		stringBuilder.append(printedStructureSymbol);

		final List<Pair<SymbolStruct, LispStruct>> allSlots = getSlots();
		for (final Pair<SymbolStruct, LispStruct> slot : allSlots) {
			final SymbolStruct slotSymbol = slot.getLeft();
			final LispStruct slotValue = slot.getRight();

			stringBuilder.append(" :");
			final String printedSlotSymbol = slotSymbol.toString();
			stringBuilder.append(printedSlotSymbol);

			stringBuilder.append(' ');
			final String printedSlotValue = slotValue.toString();
			stringBuilder.append(printedSlotValue);
		}

		stringBuilder.append(')');

		return stringBuilder.toString();
	}
}
