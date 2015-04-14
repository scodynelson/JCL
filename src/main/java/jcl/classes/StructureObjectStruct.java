package jcl.classes;

import java.util.Map;

import jcl.LispStruct;
import jcl.LispType;
import jcl.functions.FunctionStruct;
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

	protected final Map<SymbolStruct<?>, LispStruct> slots;

	protected final FunctionStruct printer;

	public StructureObjectStruct(final StructureClassStruct structureClass, final Map<SymbolStruct<?>, LispStruct> slots) {
		this(structureClass, slots, null);
	}

	public StructureObjectStruct(final StructureClassStruct structureClass, final Map<SymbolStruct<?>, LispStruct> slots,
	                             final FunctionStruct printer) {
		this.structureClass = structureClass;
		this.slots = slots;
		this.printer = printer;
	}

	public StructureClassStruct getStructureClass() {
		return structureClass;
	}

	public Map<SymbolStruct<?>, LispStruct> getSlots() {
		return slots;
	}

	public LispStruct getSlot(final SymbolStruct<?> slotName) {
		return slots.get(slotName);
	}

	public void setSlot(final SymbolStruct<?> slotName, final LispStruct newSlotValue) {
		slots.put(slotName, newSlotValue);
	}

	public FunctionStruct getPrinter() {
		return printer;
	}

	@Override
	public LispType getType() {
		return structureClass.getType();
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(structureClass)
		                            .append(slots)
		                            .append(printer)
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
		                          .append(slots, rhs.slots)
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(structureClass)
		                                                                .append(slots)
		                                                                .append(printer)
		                                                                .toString();
	}
}
