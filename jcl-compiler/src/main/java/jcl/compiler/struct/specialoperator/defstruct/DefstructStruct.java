/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.defstruct;

import java.util.List;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.classes.StructureClassStruct;

public class DefstructStruct extends CompilerSpecialOperatorStruct {

	private final SymbolStruct structureSymbol;

	private final StructureClassStruct includeStructureClass;

	private final SymbolStruct defaultConstructorSymbol;

	private final SymbolStruct printerSymbol;

	private final List<SymbolStruct> slots;

	public DefstructStruct(final SymbolStruct structureSymbol, final StructureClassStruct includeStructureClass,
	                       final SymbolStruct defaultConstructorSymbol, final SymbolStruct printerSymbol,
	                       final List<SymbolStruct> slots) {
		this.structureSymbol = structureSymbol;
		this.includeStructureClass = includeStructureClass;
		this.defaultConstructorSymbol = defaultConstructorSymbol;
		this.printerSymbol = printerSymbol;
		this.slots = slots;
	}

	public SymbolStruct getStructureSymbol() {
		return structureSymbol;
	}

	public StructureClassStruct getIncludeStructureClass() {
		return includeStructureClass;
	}

	public SymbolStruct getDefaultConstructorSymbol() {
		return defaultConstructorSymbol;
	}

	public SymbolStruct getPrinterSymbol() {
		return printerSymbol;
	}

	public List<SymbolStruct> getSlots() {
		return slots;
	}
}
