/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator.declare;

import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SpecialDeclarationStruct implements DeclarationStruct {

	private static final long serialVersionUID = 7353657327204677544L;

	private final SymbolStruct<?> var;

	public SpecialDeclarationStruct(final SymbolStruct<?> var) {
		this.var = var;
	}

	public SymbolStruct<?> getVar() {
		return var;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(var)
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
		final SpecialDeclarationStruct rhs = (SpecialDeclarationStruct) obj;
		return new EqualsBuilder().append(var, rhs.var)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(var)
		                                                                .toString();
	}
}
