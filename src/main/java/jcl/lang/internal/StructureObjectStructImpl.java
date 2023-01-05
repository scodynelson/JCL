package jcl.lang.internal;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.StructureObjectStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.classes.StructureClassStruct;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.statics.CommonLispSymbols;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

/**
 * The {@link StructureObjectStructImpl} is the object representation of a Lisp 'structure-object' type.
 */
public class StructureObjectStructImpl extends LispStructImpl implements StructureObjectStruct {

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

	@Override
	public StructureClassStruct getStructureClass() {
		return structureClass;
	}

	@Override
	public SymbolStruct getStructureSymbol() {
		return structureSymbol;
	}

	@Override
	public StructureObjectStruct getParentStructure() {
		return parentStructure;
	}

	@Override
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

	@Override
	public LispStruct getSlot(final SymbolStruct slotName) {
		if (slots.containsKey(slotName)) {
			return slots.get(slotName);
		}

		throw new SimpleErrorException("Slot " + slotName + " is not present for structure " + this);
	}

	@Override
	public void setSlot(final SymbolStruct slotName, final LispStruct newSlotValue) {
		if (slots.containsKey(slotName)) {
			slots.put(slotName, newSlotValue);
			return;
		}

		throw new SimpleErrorException("Slot " + slotName + " is not present for structure " + this);
	}

	@Override
	public boolean equalp(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof final StructureObjectStructImpl o) {
			if (!structureClass.eq(o.structureClass)) {
				return false;
			}
			for (final Map.Entry<SymbolStruct, LispStruct> entry : slots.entrySet()) {
				final SymbolStruct key = entry.getKey();
				final LispStruct value = entry.getValue();
				final LispStruct objectValue = o.slots.get(key);

				if (!value.equalp(objectValue)) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

//	@Override
//	public LispStruct typeOf() {
//		return structureClass.typeOf();
//	}

	@Override
	public LispStruct typeOf() {
		return structureClass.getClassName();
	}

	@Override
	public ClassStruct classOf() {
		return structureClass;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier instanceof StructureClassStruct) {
			final boolean isClassPresent = structureClass.getClassPrecedenceList()
			                                             .stream()
			                                             .anyMatch(classStruct -> classStruct.eq(typeSpecifier));
			return isClassPresent ? TStruct.INSTANCE : NILStruct.INSTANCE;
		}
		if (typeSpecifier == structureClass.getClassName()) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.STRUCTURE_OBJECT) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.STRUCTURE_OBJECT) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier instanceof SymbolStruct) {
			final ClassStruct classStruct = ClassStruct.findClass((SymbolStruct) typeSpecifier);
			if (classStruct != null) {
				final boolean isClassPresent = structureClass.getClassPrecedenceList()
				                                             .stream()
				                                             .anyMatch(classStruct::eq);
				return isClassPresent ? TStruct.INSTANCE : NILStruct.INSTANCE;
			}
		}
		return super.typep(typeSpecifier);
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
