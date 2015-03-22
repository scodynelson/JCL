/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.struct.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class ReturnFromStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = -6095397540807480492L;

	private final SymbolStruct<?> name;

	private final LispStruct result;

	public ReturnFromStruct(final SymbolStruct<?> name, final LispStruct result) {
		this.name = name;
		this.result = result;
	}

	public SymbolStruct<?> getName() {
		return name;
	}

	public LispStruct getResult() {
		return result;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(name)
		                            .append(result)
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
		final ReturnFromStruct rhs = (ReturnFromStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(name, rhs.name)
		                          .append(result, rhs.result)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(name)
		                                                                .append(result)
		                                                                .toString();
	}
}
