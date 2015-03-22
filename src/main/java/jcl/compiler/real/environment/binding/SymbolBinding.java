/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispType;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.allocation.Allocation;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SymbolBinding<A extends Allocation> extends Binding<A> {

	private static final long serialVersionUID = -3462756070576114237L;

	private final Environment binding;

	protected SymbolBinding(final SymbolStruct<?> symbolStruct, final A allocation, final LispType type,
	                        final Environment binding) {
		super(symbolStruct, allocation, type);
		this.binding = binding;
	}

	public Environment getBinding() {
		return binding;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(binding)
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
		final SymbolBinding<?> rhs = (SymbolBinding) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(binding, rhs.binding)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(binding)
		                                                                .toString();
	}
}
