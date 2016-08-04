/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator.defstruct;

import java.util.List;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.StructureClassStruct;
import jcl.lang.SymbolStructImpl;

public class DefstructStruct extends CompilerSpecialOperatorStruct {

	private final SymbolStructImpl structureSymbol;

	private final StructureClassStruct includeStructureClass;

	private final SymbolStructImpl defaultConstructorSymbol;

	private final SymbolStructImpl printerSymbol;

	private final List<SymbolStructImpl> slots;

	public DefstructStruct(final SymbolStructImpl structureSymbol, final StructureClassStruct includeStructureClass,
	                       final SymbolStructImpl defaultConstructorSymbol, final SymbolStructImpl printerSymbol,
	                       final List<SymbolStructImpl> slots) {
		this.structureSymbol = structureSymbol;
		this.includeStructureClass = includeStructureClass;
		this.defaultConstructorSymbol = defaultConstructorSymbol;
		this.printerSymbol = printerSymbol;
		this.slots = slots;
	}

	public SymbolStructImpl getStructureSymbol() {
		return structureSymbol;
	}

	public StructureClassStruct getIncludeStructureClass() {
		return includeStructureClass;
	}

	public SymbolStructImpl getDefaultConstructorSymbol() {
		return defaultConstructorSymbol;
	}

	public SymbolStructImpl getPrinterSymbol() {
		return printerSymbol;
	}

	public List<SymbolStructImpl> getSlots() {
		return slots;
	}
}
