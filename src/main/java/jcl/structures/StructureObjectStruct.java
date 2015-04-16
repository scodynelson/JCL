package jcl.structures;

import java.util.LinkedHashMap;
import java.util.Map;

import jcl.LispStruct;
import jcl.LispType;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link StructureObjectStruct} is the object representation of a Lisp 'structure-object' type.
 */
public class StructureObjectStruct implements LispStruct {

	private static final long serialVersionUID = 5766790087319221572L;

	protected final StructureClassStruct structureClass;

	protected final SymbolStruct<?> structureSymbol;

	protected final StructureObjectStruct parentStructure;

	protected final Map<SymbolStruct<?>, LispStruct> slots = new LinkedHashMap<>();

	protected StructureObjectStruct(final StructureClassStruct structureClass, final SymbolStruct<?> structureSymbol,
	                                final StructureObjectStruct parentStructure) {
		this.structureClass = structureClass;
		this.structureSymbol = structureSymbol;
		this.parentStructure = parentStructure;
	}

	public StructureClassStruct getStructureClass() {
		return structureClass;
	}

	public SymbolStruct<?> getStructureSymbol() {
		return structureSymbol;
	}

	public StructureObjectStruct getParentStructure() {
		return parentStructure;
	}

	public Map<SymbolStruct<?>, LispStruct> getSlots() {
		final Map<SymbolStruct<?>, LispStruct> allSlots = new LinkedHashMap<>();

		if (parentStructure != null) {
			final Map<SymbolStruct<?>, LispStruct> parentAllSlots = parentStructure.getSlots();
			allSlots.putAll(parentAllSlots);
		}

		allSlots.putAll(slots);
		return allSlots;
	}

	public LispStruct getSlot(final SymbolStruct<?> slotName) {
		if (slots.containsKey(slotName)) {
			return slots.get(slotName);
		}

		throw new SimpleErrorException("Slot " + slotName + " is not present for structure " + this);
	}

	public void setSlot(final SymbolStruct<?> slotName, final LispStruct newSlotValue) {
		if (slots.containsKey(slotName)) {
			slots.put(slotName, newSlotValue);
		}

		throw new SimpleErrorException("Slot " + slotName + " is not present for structure " + this);
	}

	@Override
	public LispType getType() {
		return structureClass.getType();
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(structureClass)
		                            .append(structureSymbol)
		                            .append(parentStructure)
		                            .append(slots)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final StructureObjectStruct rhs = (StructureObjectStruct) obj;
		return new EqualsBuilder().append(structureClass, rhs.structureClass)
		                          .append(structureSymbol, rhs.structureSymbol)
		                          .append(parentStructure, rhs.parentStructure)
		                          .append(slots, rhs.slots)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(structureClass)
		                                                                .append(structureSymbol)
		                                                                .append(parentStructure)
		                                                                .append(slots)
		                                                                .toString();
	}
}
