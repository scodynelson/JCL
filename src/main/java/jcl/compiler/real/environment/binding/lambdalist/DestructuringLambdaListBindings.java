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

public class DestructuringLambdaListBindings implements Serializable {

	private static final long serialVersionUID = -2835999831080133844L;

	private final WholeBinding wholeBinding;

	private final List<RequiredBinding> requiredBindings;

	private final List<OptionalBinding> optionalBindings;

	private final RestBinding restBinding;

	private final BodyBinding bodyBinding;

	private final List<KeyBinding> keyBindings;

	private final List<AuxBinding> auxBindings;

	private final boolean allowOtherKeys;

	public DestructuringLambdaListBindings(final WholeBinding wholeBinding, final List<RequiredBinding> requiredBindings,
	                                       final List<OptionalBinding> optionalBindings, final RestBinding restBinding,
	                                       final BodyBinding bodyBinding, final List<KeyBinding> keyBindings,
	                                       final List<AuxBinding> auxBindings, final boolean allowOtherKeys) {
		this.wholeBinding = wholeBinding;
		this.requiredBindings = requiredBindings;
		this.optionalBindings = optionalBindings;
		this.restBinding = restBinding;
		this.bodyBinding = bodyBinding;
		this.keyBindings = keyBindings;
		this.auxBindings = auxBindings;
		this.allowOtherKeys = allowOtherKeys;
	}

	public WholeBinding getWholeBinding() {
		return wholeBinding;
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

	public BodyBinding getBodyBinding() {
		return bodyBinding;
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
		return new HashCodeBuilder().append(wholeBinding)
		                            .append(requiredBindings)
		                            .append(optionalBindings)
		                            .append(restBinding)
		                            .append(bodyBinding)
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
		final DestructuringLambdaListBindings rhs = (DestructuringLambdaListBindings) obj;
		return new EqualsBuilder().append(wholeBinding, rhs.wholeBinding)
		                          .append(requiredBindings, rhs.requiredBindings)
		                          .append(optionalBindings, rhs.optionalBindings)
		                          .append(restBinding, rhs.restBinding)
		                          .append(bodyBinding, rhs.bodyBinding)
		                          .append(keyBindings, rhs.keyBindings)
		                          .append(auxBindings, rhs.auxBindings)
		                          .append(allowOtherKeys, rhs.allowOtherKeys)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(wholeBinding)
		                                                                .append(requiredBindings)
		                                                                .append(optionalBindings)
		                                                                .append(restBinding)
		                                                                .append(bodyBinding)
		                                                                .append(keyBindings)
		                                                                .append(auxBindings)
		                                                                .append(allowOtherKeys)
		                                                                .toString();
	}
}
