/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.ParameterBinding;
import jcl.symbols.SymbolStruct;
import jcl.types.TType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class OptionalBinding extends ParameterBinding {

	private static final long serialVersionUID = 3357381481589151323L;

	private final SuppliedPBinding suppliedPBinding;

	public OptionalBinding(final SymbolStruct<?> symbolStruct, final LispStruct initForm,
	                       final SuppliedPBinding suppliedPBinding) {
		super(symbolStruct, TType.INSTANCE, initForm);
		this.suppliedPBinding = suppliedPBinding;
	}

	public SuppliedPBinding getSuppliedPBinding() {
		return suppliedPBinding;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(suppliedPBinding)
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
		final OptionalBinding rhs = (OptionalBinding) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(suppliedPBinding, rhs.suppliedPBinding)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(suppliedPBinding)
		                                                                .toString();
	}
}
