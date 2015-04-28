/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer.lambdalistparser;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

final class KeyParseResult extends ParseResult {

	private final List<KeyBinding> keyBindings;

	KeyParseResult(final LispStruct currentElement, final List<KeyBinding> keyBindings) {
		super(currentElement);
		this.keyBindings = keyBindings;
	}

	List<KeyBinding> getKeyBindings() {
		return keyBindings;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(keyBindings)
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
		final KeyParseResult rhs = (KeyParseResult) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(keyBindings, rhs.keyBindings)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(keyBindings)
		                                                                .toString();
	}
}
