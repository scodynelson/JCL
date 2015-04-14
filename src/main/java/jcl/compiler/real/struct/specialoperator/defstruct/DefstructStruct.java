/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.defstruct;

import java.util.Map;

import jcl.LispStruct;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class DefstructStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = -7747095883491636502L;

	private final SymbolStruct<?> symbol;

	private final SymbolStruct<?> includeSymbol;

	private final LambdaStruct defaultConstructor;

	private final LambdaStruct printer;

	private final Map<SymbolStruct<?>, LispStruct> slots;

	public DefstructStruct(final SymbolStruct<?> symbol, final SymbolStruct<?> includeSymbol,
	                       final LambdaStruct defaultConstructor, final LambdaStruct printer,
	                       final Map<SymbolStruct<?>, LispStruct> slots) {
		this.symbol = symbol;
		this.includeSymbol = includeSymbol;
		this.defaultConstructor = defaultConstructor;
		this.printer = printer;
		this.slots = slots;
	}

	public SymbolStruct<?> getSymbol() {
		return symbol;
	}

	public SymbolStruct<?> getIncludeSymbol() {
		return includeSymbol;
	}

	public LambdaStruct getDefaultConstructor() {
		return defaultConstructor;
	}

	public LambdaStruct getPrinter() {
		return printer;
	}

	public Map<SymbolStruct<?>, LispStruct> getSlots() {
		return slots;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(symbol)
		                            .append(includeSymbol)
		                            .append(defaultConstructor)
		                            .append(printer)
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
		                          .append(symbol, rhs.symbol)
		                          .append(includeSymbol, rhs.includeSymbol)
		                          .append(defaultConstructor, rhs.defaultConstructor)
		                          .append(printer, rhs.printer)
		                          .append(slots, rhs.slots)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(symbol)
		                                                                .append(includeSymbol)
		                                                                .append(defaultConstructor)
		                                                                .append(printer)
		                                                                .append(slots)
		                                                                .toString();
	}
}
