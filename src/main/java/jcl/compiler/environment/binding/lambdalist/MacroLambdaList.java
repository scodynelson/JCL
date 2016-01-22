/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

public class MacroLambdaList implements Serializable {

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

	public MacroLambdaList(final WholeParameter wholeBinding, final EnvironmentParameter environmentBinding,
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

	private MacroLambdaList(final Builder builder) {
		wholeBinding = builder.wholeBinding;
		environmentBinding = builder.environmentBinding;
		requiredBindings = builder.requiredBindings;
		optionalBindings = builder.optionalBindings;
		restBinding = builder.restBinding;
		bodyBinding = builder.bodyBinding;
		keyBindings = builder.keyBindings;
		auxBindings = builder.auxBindings;
		allowOtherKeys = builder.allowOtherKeys;
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

	public static Builder builder() {
		return new Builder();
	}

	public static final class Builder {

		private WholeParameter wholeBinding;

		private EnvironmentParameter environmentBinding;

		private List<RequiredParameter> requiredBindings = Collections.emptyList();

		private List<OptionalParameter> optionalBindings = Collections.emptyList();

		private RestParameter restBinding;

		private BodyParameter bodyBinding;

		private List<KeyParameter> keyBindings = Collections.emptyList();

		private List<AuxParameter> auxBindings = Collections.emptyList();

		private boolean allowOtherKeys;

		private Builder() {
		}

		public Builder wholeBinding(final WholeParameter wholeBinding) {
			this.wholeBinding = wholeBinding;
			return this;
		}

		public Builder environmentBinding(final EnvironmentParameter environmentBinding) {
			this.environmentBinding = environmentBinding;
			return this;
		}

		public Builder requiredBindings(final List<RequiredParameter> requiredBindings) {
			this.requiredBindings = requiredBindings;
			return this;
		}

		public Builder optionalBindings(final List<OptionalParameter> optionalBindings) {
			this.optionalBindings = optionalBindings;
			return this;
		}

		public Builder restBinding(final RestParameter restBinding) {
			this.restBinding = restBinding;
			return this;
		}

		public Builder bodyBinding(final BodyParameter bodyBinding) {
			this.bodyBinding = bodyBinding;
			return this;
		}

		public Builder keyBindings(final List<KeyParameter> keyBindings) {
			this.keyBindings = keyBindings;
			return this;
		}

		public Builder auxBindings(final List<AuxParameter> auxBindings) {
			this.auxBindings = auxBindings;
			return this;
		}

		public Builder allowOtherKeys(final boolean allowOtherKeys) {
			this.allowOtherKeys = allowOtherKeys;
			return this;
		}

		public MacroLambdaList build() {
			return new MacroLambdaList(this);
		}
	}
}
