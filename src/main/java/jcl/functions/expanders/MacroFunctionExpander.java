/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.expanders;

import java.util.List;

import jcl.LispStruct;
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
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;

public abstract class MacroFunctionExpander<O extends LispStruct> extends MacroExpander<O, ListStruct> {

	protected MacroLambdaList macroLambdaListBindings;

	protected MacroFunctionExpander() {
		// TODO: Remove eventually
		this("");
	}

	protected MacroFunctionExpander(final String documentation) {
		super(documentation);
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		final SymbolStruct functionSymbol = getFunctionSymbol();
		functionSymbol.setMacroFunctionExpander(this);
	}

	protected WholeParameter getWholeBinding() {
		return new WholeParameter(new SymbolStruct("temp_whole_" + System.nanoTime()));
	}

	protected EnvironmentParameter getEnvironmentBinding() {
		return new EnvironmentParameter(new SymbolStruct("temp_environment_" + System.nanoTime()));
	}

	protected BodyParameter getBodyBinding() {
		return null;
	}

	@Override
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
