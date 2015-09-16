/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.symbols.SymbolStruct;

public class InnerLambdaEnvironment extends LambdaEnvironment {

	private static final long serialVersionUID = -5882720704455871085L;

	private final List<EnvironmentParameterBinding> functionBindings = new ArrayList<>();

	public InnerLambdaEnvironment(final Environment parent) {
		super(parent);
	}

	public List<EnvironmentParameterBinding> getFunctionBindings() {
		return functionBindings;
	}

	public boolean hasFunctionBinding(final SymbolStruct<?> symbolStruct) {
		return functionBindings.stream()
		                       .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public Optional<EnvironmentParameterBinding> getFunctionBinding(final SymbolStruct<?> symbolStruct) {
		return functionBindings.stream()
		                       .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		                       .findFirst();
	}

	public void addFunctionBinding(final EnvironmentParameterBinding functionBinding) {
		functionBindings.add(functionBinding);
	}
}
