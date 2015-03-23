/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SymbolMacroBinding extends Binding<ParameterAllocation> {

	private static final long serialVersionUID = -7630096026388828215L;

	private final LispStruct expansion;

	public SymbolMacroBinding(final SymbolStruct<?> symbolStruct, final ParameterAllocation allocation, final LispType type,
	                          final LispStruct expansion) {
		super(symbolStruct, allocation, type);
		this.expansion = expansion;
	}

	public LispStruct getExpansion() {
		return expansion;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(expansion)
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
		final SymbolMacroBinding rhs = (SymbolMacroBinding) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(expansion, rhs.expansion)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(expansion)
		                                                                .toString();
	}
}
