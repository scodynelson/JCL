/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.function.expanders;

import java.util.Collections;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.BodyParameter;
import jcl.compiler.environment.binding.lambdalist.EnvironmentParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.MacroLambdaList;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.environment.binding.lambdalist.WholeParameter;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.expander.MacroFunctionExpanderInter;

public abstract class MacroFunctionExpander<O extends LispStruct> extends MacroExpander<O, ListStruct> implements MacroFunctionExpanderInter {

	protected MacroLambdaList macroLambdaListBindings;

	protected MacroFunctionExpander() {
	}

	protected MacroFunctionExpander(final String documentation) {
		super(documentation);
	}

	protected List<RequiredParameter> getRequiredBindings(final Environment currentEnvironment) {
		return Collections.emptyList();
	}

	protected List<OptionalParameter> getOptionalBindings(final Environment currentEnvironment) {
		return Collections.emptyList();
	}

	protected RestParameter getRestBinding(final Environment currentEnvironment) {
		return null;
	}

	protected List<KeyParameter> getKeyBindings(final Environment currentEnvironment) {
		return Collections.emptyList();
	}

	protected boolean getAllowOtherKeys(final Environment currentEnvironment) {
		return false;
	}

	protected List<AuxParameter> getAuxBindings(final Environment currentEnvironment) {
		return Collections.emptyList();
	}

	protected WholeParameter getWholeBinding(final Environment currentEnvironment) {
		return new WholeParameter(SymbolStruct.toLispSymbol("temp_whole_" + System.nanoTime()));
	}

	protected EnvironmentParameter getEnvironmentBinding(final Environment currentEnvironment) {
		return new EnvironmentParameter(SymbolStruct.toLispSymbol("temp_environment_" + System.nanoTime()));
	}

	protected BodyParameter getBodyBinding(final Environment currentEnvironment) {
		return null;
	}

	protected void initLambdaListBindings(final Environment currentEnvironment) {
		final WholeParameter wholeBinding = getWholeBinding(currentEnvironment);
		final List<RequiredParameter> requiredBindings = getRequiredBindings(currentEnvironment);
		final List<OptionalParameter> optionalBindings = getOptionalBindings(currentEnvironment);
		final RestParameter restBinding = getRestBinding(currentEnvironment);
		final BodyParameter bodyBinding = getBodyBinding(currentEnvironment);
		final List<KeyParameter> keyBindings = getKeyBindings(currentEnvironment);
		final boolean allowOtherKeys = getAllowOtherKeys(currentEnvironment);
		final List<AuxParameter> auxBindings = getAuxBindings(currentEnvironment);
		final EnvironmentParameter environmentBinding = getEnvironmentBinding(currentEnvironment);
		macroLambdaListBindings = MacroLambdaList.builder()
		                                         .wholeBinding(wholeBinding)
		                                         .environmentBinding(environmentBinding)
		                                         .requiredBindings(requiredBindings)
		                                         .optionalBindings(optionalBindings)
		                                         .restBinding(restBinding)
		                                         .bodyBinding(bodyBinding)
		                                         .keyBindings(keyBindings)
		                                         .allowOtherKeys(allowOtherKeys)
		                                         .auxBindings(auxBindings)
		                                         .build();
	}

	@Override
	public abstract O expand(final ListStruct form, final Environment environment);

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		final ListStruct listStruct = (ListStruct) lispStructs[0];
		final Environment environment = (Environment) lispStructs[1];
		return expand(listStruct, environment);
	}
}
