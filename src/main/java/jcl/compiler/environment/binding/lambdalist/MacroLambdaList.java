/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class MacroLambdaList {

	private final WholeParameter wholeBinding;
	private final EnvironmentParameter environmentBinding;
	private final List<RequiredParameter> requiredBindings;
	private final List<OptionalParameter> optionalBindings;
	private final RestParameter restBinding;
	private final BodyParameter bodyBinding;
	private final List<KeyParameter> keyBindings;
	private final List<AuxParameter> auxBindings;
	private final boolean allowOtherKeys;

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

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();
		builder.append('(');

		boolean wasBuilderUpdated = false;
		if (!requiredBindings.isEmpty()) {
			final String requiredString = requiredBindings.stream()
			                                              .map(Object::toString)
			                                              .collect(Collectors.joining(" "));
			builder.append(requiredString);

			wasBuilderUpdated = true;
		}
		if (!optionalBindings.isEmpty()) {
			if (wasBuilderUpdated) {
				builder.append(' ');
			}

			final String optionalString = optionalBindings.stream()
			                                              .map(Object::toString)
			                                              .collect(Collectors.joining(" "));
			builder.append("&optional ")
			       .append(optionalString);

			wasBuilderUpdated = true;
		}

		if (restBinding != null) {
			if (wasBuilderUpdated) {
				builder.append(' ');
			}

			final String restString = restBinding.toString();
			builder.append("&rest ")
			       .append(restString);

			wasBuilderUpdated = true;
		}

		if (!keyBindings.isEmpty()) {
			if (wasBuilderUpdated) {
				builder.append(' ');
			}

			final String keyString = keyBindings.stream()
			                                    .map(Object::toString)
			                                    .collect(Collectors.joining(" "));
			builder.append("&key ")
			       .append(keyString);

			wasBuilderUpdated = true;
		}

		if (allowOtherKeys) {
			if (wasBuilderUpdated) {
				builder.append(' ');
			}

			builder.append("&allow-other-keys");

			wasBuilderUpdated = true;
		}

		if (!auxBindings.isEmpty()) {
			if (wasBuilderUpdated) {
				builder.append(' ');
			}

			final String auxString = auxBindings.stream()
			                                    .map(Object::toString)
			                                    .collect(Collectors.joining(" "));
			builder.append(auxString);
		}

		builder.append(')');
		return builder.toString();
	}
}
