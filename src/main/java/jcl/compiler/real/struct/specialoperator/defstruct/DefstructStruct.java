/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.defstruct;

import java.util.List;

import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class DefstructStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = -7747095883491636502L;

	private final SymbolStruct<?> structureSymbol;

	private final SymbolStruct<?> includeStructureSymbol;

	private final SymbolStruct<?> defaultConstructorSymbol;

	private final SymbolStruct<?> printerSymbol;

	private final List<SymbolStruct<?>> slots;

	public DefstructStruct(final SymbolStruct<?> structureSymbol, final SymbolStruct<?> includeStructureSymbol,
	                       final SymbolStruct<?> defaultConstructorSymbol, final SymbolStruct<?> printerSymbol,
	                       final List<SymbolStruct<?>> slots) {
		this.structureSymbol = structureSymbol;
		this.includeStructureSymbol = includeStructureSymbol;
		this.defaultConstructorSymbol = defaultConstructorSymbol;
		this.printerSymbol = printerSymbol;
		this.slots = slots;
	}

	public SymbolStruct<?> getStructureSymbol() {
		return structureSymbol;
	}

	public SymbolStruct<?> getIncludeStructureSymbol() {
		return includeStructureSymbol;
	}

	public SymbolStruct<?> getDefaultConstructorSymbol() {
		return defaultConstructorSymbol;
	}

	public SymbolStruct<?> getPrinterSymbol() {
		return printerSymbol;
	}

	public List<SymbolStruct<?>> getSlots() {
		return slots;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(structureSymbol)
		                            .append(includeStructureSymbol)
		                            .append(defaultConstructorSymbol)
		                            .append(printerSymbol)
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
		final DefstructStruct rhs = (DefstructStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(structureSymbol, rhs.structureSymbol)
		                          .append(includeStructureSymbol, rhs.includeStructureSymbol)
		                          .append(defaultConstructorSymbol, rhs.defaultConstructorSymbol)
		                          .append(printerSymbol, rhs.printerSymbol)
		                          .append(slots, rhs.slots)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(structureSymbol)
		                                                                .append(includeStructureSymbol)
		                                                                .append(defaultConstructorSymbol)
		                                                                .append(printerSymbol)
		                                                                .append(slots)
		                                                                .toString();
	}
}
