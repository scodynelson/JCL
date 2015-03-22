/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import java.io.Serializable;
import java.util.List;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class OrdinaryLambdaListBindings implements Serializable {

	private static final long serialVersionUID = 3980582571605022674L;

	private final List<RequiredBinding> requiredBindings;

	private final List<OptionalBinding> optionalBindings;

	private final RestBinding restBinding;

	private final List<KeyBinding> keyBindings;

	private final List<AuxBinding> auxBindings;

	private final boolean allowOtherKeys;

	public OrdinaryLambdaListBindings(final List<RequiredBinding> requiredBindings, final List<OptionalBinding> optionalBindings,
	                                  final RestBinding restBinding, final List<KeyBinding> keyBindings, final List<AuxBinding> auxBindings,
	                                  final boolean allowOtherKeys) {
		this.requiredBindings = requiredBindings;
		this.optionalBindings = optionalBindings;
		this.restBinding = restBinding;
		this.keyBindings = keyBindings;
		this.auxBindings = auxBindings;
		this.allowOtherKeys = allowOtherKeys;
	}

	public List<RequiredBinding> getRequiredBindings() {
		return requiredBindings;
	}

	public List<OptionalBinding> getOptionalBindings() {
		return optionalBindings;
	}

	public RestBinding getRestBinding() {
		return restBinding;
	}

	public List<KeyBinding> getKeyBindings() {
		return keyBindings;
	}

	public List<AuxBinding> getAuxBindings() {
		return auxBindings;
	}

	public boolean isAllowOtherKeys() {
		return allowOtherKeys;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(requiredBindings)
		                            .append(optionalBindings)
		                            .append(restBinding)
		                            .append(keyBindings)
		                            .append(auxBindings)
		                            .append(allowOtherKeys)
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
		final OrdinaryLambdaListBindings rhs = (OrdinaryLambdaListBindings) obj;
		return new EqualsBuilder().append(requiredBindings, rhs.requiredBindings)
		                          .append(optionalBindings, rhs.optionalBindings)
		                          .append(restBinding, rhs.restBinding)
		                          .append(keyBindings, rhs.keyBindings)
		                          .append(auxBindings, rhs.auxBindings)
		                          .append(allowOtherKeys, rhs.allowOtherKeys)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(requiredBindings)
		                                                                .append(optionalBindings)
		                                                                .append(restBinding)
		                                                                .append(keyBindings)
		                                                                .append(auxBindings)
		                                                                .append(allowOtherKeys)
		                                                                .toString();
	}
}
