/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment.binding.lambdalist;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class DestructuringLambdaList {

	private final WholeParameter wholeBinding;
	private final List<RequiredParameter> requiredBindings;
	private final List<OptionalParameter> optionalBindings;
	private final RestParameter restBinding;
	private final BodyParameter bodyBinding;
	private final List<KeyParameter> keyBindings;
	private final List<AuxParameter> auxBindings;
	private final boolean allowOtherKeys;
}
