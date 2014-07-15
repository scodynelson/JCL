package jcl.compiler.real.environment.lambdalist;

import java.util.List;

public class OriginalLambdaListBindings {

	private final List<RequiredBinding> requiredBindings;
	private final List<OptionalBinding> optionalBindings;
	private final RestBinding restBinding;
	private final List<KeyBinding> keyBindings;
	private final List<AuxBinding> auxBindings;

	private final boolean allowOtherKeys;

	public OriginalLambdaListBindings(final List<RequiredBinding> requiredBindings, final List<OptionalBinding> optionalBindings,
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
}
