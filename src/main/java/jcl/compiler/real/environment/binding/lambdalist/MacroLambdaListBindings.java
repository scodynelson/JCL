/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import java.io.Serializable;
import java.util.List;

public class MacroLambdaListBindings implements Serializable {

	private static final long serialVersionUID = 5857625809333286733L;

	private final WholeBinding wholeBinding;

	private final EnvironmentBinding environmentBinding;

	private final List<RequiredBinding> requiredBindings;

	private final List<OptionalBinding> optionalBindings;

	private final RestBinding restBinding;

	private final BodyBinding bodyBinding;

	private final List<KeyBinding> keyBindings;

	private final List<AuxBinding> auxBindings;

	private final boolean allowOtherKeys;

	public MacroLambdaListBindings(final WholeBinding wholeBinding, final EnvironmentBinding environmentBinding,
	                               final List<RequiredBinding> requiredBindings, final List<OptionalBinding> optionalBindings,
	                               final RestBinding restBinding, final BodyBinding bodyBinding,
	                               final List<KeyBinding> keyBindings, final List<AuxBinding> auxBindings,
	                               final boolean allowOtherKeys) {
		this.wholeBinding = wholeBinding;
		this.environmentBinding = environmentBinding;
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

	public EnvironmentBinding getEnvironmentBinding() {
		return environmentBinding;
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
}
