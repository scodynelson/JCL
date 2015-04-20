/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispStruct;
import jcl.LispType;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class EnvironmentParameterBinding extends EnvironmentBinding {

	private static final long serialVersionUID = -4911545990664188285L;

	private final LispStruct initForm;

	public EnvironmentParameterBinding(final SymbolStruct<?> symbolStruct, final LispType type,
	                                   final LispStruct initForm) {
		super(symbolStruct, type);
		this.initForm = initForm;
	}

	public LispStruct getInitForm() {
		return initForm;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(initForm)
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
		final EnvironmentParameterBinding rhs = (EnvironmentParameterBinding) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(initForm, rhs.initForm)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(initForm)
		                                                                .toString();
	}
}
