/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment.binding.lambdalist;

import java.io.Serializable;
import java.util.List;

public class MacroLambdaListBindings implements Serializable {

	private static final long serialVersionUID = 5857625809333286733L;

	private final WholeParameter wholeBinding;

	private final EnvironmentParameter environmentBinding;

	private final List<RequiredParameter> requiredBindings;

	private final List<OptionalParameter> optionalBindings;

	private final RestParameter restBinding;

	private final BodyParameter bodyBinding;

	private final List<KeyParameter> keyBindings;

	private final List<AuxParameter> auxBindings;

	private final boolean allowOtherKeys;

	public MacroLambdaListBindings(final WholeParameter wholeBinding, final EnvironmentParameter environmentBinding,
	                               final List<RequiredParameter> requiredBindings, final List<OptionalParameter> optionalBindings,
	                               final RestParameter restBinding, final BodyParameter bodyBinding,
	                               final List<KeyParameter> keyBindings, final List<AuxParameter> auxBindings,
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

	public WholeParameter getWholeBinding() {
		return wholeBinding;
	}

	public EnvironmentParameter getEnvironmentBinding() {
		return environmentBinding;
	}

	public List<RequiredParameter> getRequiredBindings() {
		return requiredBindings;
	}

	public List<OptionalParameter> getOptionalBindings() {
		return optionalBindings;
	}

	public RestParameter getRestBinding() {
		return restBinding;
	}

	public BodyParameter getBodyBinding() {
		return bodyBinding;
	}

	public List<KeyParameter> getKeyBindings() {
		return keyBindings;
	}

	public List<AuxParameter> getAuxBindings() {
		return auxBindings;
	}

	public boolean isAllowOtherKeys() {
		return allowOtherKeys;
	}
}
