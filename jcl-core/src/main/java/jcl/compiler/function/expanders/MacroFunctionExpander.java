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

	protected List<RequiredParameter> getRequiredBindings() {
		return Collections.emptyList();
	}

	protected List<OptionalParameter> getOptionalBindings() {
		return Collections.emptyList();
	}

	protected RestParameter getRestBinding() {
		return null;
	}

	protected List<KeyParameter> getKeyBindings() {
		return Collections.emptyList();
	}

	protected boolean getAllowOtherKeys() {
		return false;
	}

	protected List<AuxParameter> getAuxBindings() {
		return Collections.emptyList();
	}

	protected WholeParameter getWholeBinding() {
		return new WholeParameter(SymbolStruct.toLispSymbol("temp_whole_" + System.nanoTime()));
	}

	protected EnvironmentParameter getEnvironmentBinding() {
		return new EnvironmentParameter(SymbolStruct.toLispSymbol("temp_environment_" + System.nanoTime()));
	}

	protected BodyParameter getBodyBinding() {
		return null;
	}

	protected void initLambdaListBindings() {
		final WholeParameter wholeBinding = getWholeBinding();
		final List<RequiredParameter> requiredBindings = getRequiredBindings();
		final List<OptionalParameter> optionalBindings = getOptionalBindings();
		final RestParameter restBinding = getRestBinding();
		final BodyParameter bodyBinding = getBodyBinding();
		final List<KeyParameter> keyBindings = getKeyBindings();
		final boolean allowOtherKeys = getAllowOtherKeys();
		final List<AuxParameter> auxBindings = getAuxBindings();
		final EnvironmentParameter environmentBinding = getEnvironmentBinding();
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
