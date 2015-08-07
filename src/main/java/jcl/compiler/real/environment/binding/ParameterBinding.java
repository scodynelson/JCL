/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding;

import jcl.LispStruct;
import jcl.LispType;
import jcl.compiler.real.environment.binding.lambdalist.DestructuringLambdaListBindings;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class ParameterBinding extends Binding {

	private static final long serialVersionUID = 4128878993186537174L;

	private final LispStruct initForm;

	private final DestructuringLambdaListBindings destructuringForm;

	private final boolean isSpecial;

	protected ParameterBinding(final SymbolStruct<?> symbolStruct, final DestructuringLambdaListBindings destructuringForm,
	                           final LispType type, final LispStruct initForm, final boolean isSpecial) {
		super(symbolStruct, type);
		this.destructuringForm = destructuringForm;
		this.initForm = initForm;
		this.isSpecial = isSpecial;
	}

	public DestructuringLambdaListBindings getDestructuringForm() {
		return destructuringForm;
	}

	public LispStruct getInitForm() {
		return initForm;
	}

	public boolean isSpecial() {
		return isSpecial;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(initForm)
		                            .append(isSpecial)
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
		final ParameterBinding rhs = (ParameterBinding) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(initForm, rhs.initForm)
		                          .append(isSpecial, rhs.isSpecial)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(initForm)
		                                                                .append(isSpecial)
		                                                                .toString();
	}
}
