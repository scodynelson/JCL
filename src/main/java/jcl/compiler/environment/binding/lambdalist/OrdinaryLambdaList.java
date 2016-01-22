/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

public class OrdinaryLambdaList implements Serializable {

	private static final long serialVersionUID = 3980582571605022674L;

	private final List<RequiredParameter> requiredBindings;

	private final List<OptionalParameter> optionalBindings;

	private final RestParameter restBinding;

	private final List<KeyParameter> keyBindings;

	private final List<AuxParameter> auxBindings;

	private final boolean allowOtherKeys;

	public OrdinaryLambdaList(final List<RequiredParameter> requiredBindings, final List<OptionalParameter> optionalBindings,
	                          final RestParameter restBinding, final List<KeyParameter> keyBindings, final List<AuxParameter> auxBindings,
	                          final boolean allowOtherKeys) {
		this.requiredBindings = requiredBindings;
		this.optionalBindings = optionalBindings;
		this.restBinding = restBinding;
		this.keyBindings = keyBindings;
		this.auxBindings = auxBindings;
		this.allowOtherKeys = allowOtherKeys;
	}

	private OrdinaryLambdaList(final Builder builder) {
		requiredBindings = builder.requiredBindings;
		optionalBindings = builder.optionalBindings;
		restBinding = builder.restBinding;
		keyBindings = builder.keyBindings;
		auxBindings = builder.auxBindings;
		allowOtherKeys = builder.allowOtherKeys;
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

		private List<RequiredParameter> requiredBindings = Collections.emptyList();

		private List<OptionalParameter> optionalBindings = Collections.emptyList();

		private RestParameter restBinding;

		private List<KeyParameter> keyBindings = Collections.emptyList();

		private List<AuxParameter> auxBindings = Collections.emptyList();

		private boolean allowOtherKeys;

		private Builder() {
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

		public OrdinaryLambdaList build() {
			return new OrdinaryLambdaList(this);
		}
	}
}
