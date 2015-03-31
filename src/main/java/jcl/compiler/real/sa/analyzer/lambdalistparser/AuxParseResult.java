/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

final class AuxParseResult extends ParseResult {

	private final List<AuxBinding> auxBindings;

	AuxParseResult(final LispStruct currentElement, final int currentPosition, final List<AuxBinding> auxBindings) {
		super(currentElement, currentPosition);
		this.auxBindings = auxBindings;
	}

	List<AuxBinding> getAuxBindings() {
		return auxBindings;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(auxBindings)
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
		final AuxParseResult rhs = (AuxParseResult) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(auxBindings, rhs.auxBindings)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(auxBindings)
		                                                                .toString();
	}
}
