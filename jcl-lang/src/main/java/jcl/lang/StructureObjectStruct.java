package jcl.lang;

import java.util.List;

import jcl.lang.classes.StructureClassStruct;
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
}
