/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.expanders;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.real.environment.binding.lambdalist.BodyParameter;
import jcl.compiler.real.environment.binding.lambdalist.EnvironmentParameter;
import jcl.compiler.real.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.real.environment.binding.lambdalist.MacroLambdaList;
import jcl.compiler.real.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.real.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.real.environment.binding.lambdalist.RestParameter;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPParameter;
import jcl.compiler.real.environment.binding.lambdalist.WholeParameter;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.FunctionParameterBinding;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.system.CommonLispSymbols;

public abstract class MacroFunctionExpander<O extends LispStruct> extends MacroExpander<O, ListStruct> {

	private static final long serialVersionUID = -4041262906159677088L;

	protected MacroLambdaList macroLambdaListBindings;

	@Override
	public void afterPropertiesSet() throws Exception {
		final SymbolStruct<?> functionSymbol = getFunctionSymbol();
		functionSymbol.setMacroFunctionExpander(this);
	}

	protected List<FunctionParameterBinding> getFunctionBindings(final LispStruct[] lispStructs) {
		// TODO: This expansion only applies for "Ordinary" function parameters. Macro??? Override or expand this???
		final List<RequiredParameter> requiredBindings = macroLambdaListBindings.getRequiredBindings();
		final List<OptionalParameter> optionalBindings = macroLambdaListBindings.getOptionalBindings();
		final RestParameter restBinding = macroLambdaListBindings.getRestBinding();
		final List<KeyParameter> keyBindings = macroLambdaListBindings.getKeyBindings();
		boolean allowOtherKeys = macroLambdaListBindings.isAllowOtherKeys();
		final List<AuxParameter> auxBindings = macroLambdaListBindings.getAuxBindings();

		final List<FunctionParameterBinding> functionParametersToBind = new ArrayList<>();

		final List<LispStruct> functionArguments = Arrays.asList(lispStructs);
		final int numberOfArguments = functionArguments.size();
		final Iterator<LispStruct> functionArgumentsIterator = functionArguments.iterator();

		final String functionClassName = getClass().getSimpleName();
		final int numberOfRequired = requiredBindings.size();
		for (final RequiredParameter requiredBinding : requiredBindings) {
			if (!functionArgumentsIterator.hasNext()) {
				throw new ProgramErrorException("Too few arguments in call to '" + functionClassName + "'. " + numberOfArguments + " arguments provided, at least " + numberOfRequired + " required.");
			}

			final SymbolStruct<?> requiredSymbol = requiredBinding.getVar();
			final LispStruct requiredInitForm = functionArgumentsIterator.next();

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(requiredSymbol, requiredInitForm, requiredBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		for (final OptionalParameter optionalBinding : optionalBindings) {
			final LispStruct optionalInitForm;
			final LispStruct suppliedPInitForm;

			if (functionArgumentsIterator.hasNext()) {
				optionalInitForm = functionArgumentsIterator.next();
				suppliedPInitForm = TStruct.INSTANCE;
			} else {
				optionalInitForm = INIT_FORM_PLACEHOLDER;
				suppliedPInitForm = NILStruct.INSTANCE;
			}
			final SymbolStruct<?> optionalSymbol = optionalBinding.getVar();

			FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(optionalSymbol, optionalInitForm, optionalBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);

			final SuppliedPParameter suppliedPBinding = optionalBinding.getSuppliedPBinding();
			final SymbolStruct<?> suppliedPSymbol = suppliedPBinding.getVar();

			functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, suppliedPInitForm, suppliedPBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		final int numberOfKeys = keyBindings.size();
		final Map<SymbolStruct<?>, KeyParameter> keysToBindings = new HashMap<>();
		for (final KeyParameter keyBinding : keyBindings) {
			final SymbolStruct<?> keyName = keyBinding.getKeyName();
			keysToBindings.put(keyName, keyBinding);
		}

		// Need to wrap the keySet() in a new HashSet because of the remove() operation below on the 'keysToBindings'
		final Set<SymbolStruct<?>> keys = new HashSet<>(keysToBindings.keySet());

		final List<LispStruct> restList = new ArrayList<>();

		while (functionArgumentsIterator.hasNext()) {
			final LispStruct nextArgument = functionArgumentsIterator.next();
			restList.add(nextArgument);
		}

		final List<SymbolStruct<?>> otherKeys = new ArrayList<>();

		final Map<SymbolStruct<?>, FunctionParameterBinding> keywordFunctionParametersToBind = new LinkedHashMap<>(keyBindings.size());
		for (final KeyParameter keyBinding : keyBindings) {
			final SymbolStruct<?> keySymbol = keyBinding.getVar();

			FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(keySymbol, INIT_FORM_PLACEHOLDER, keyBinding.isSpecial());
			keywordFunctionParametersToBind.put(keySymbol, functionParameterBinding);

			final SuppliedPParameter suppliedPBinding = keyBinding.getSuppliedPBinding();
			final SymbolStruct<?> suppliedPSymbol = suppliedPBinding.getVar();

			functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, NILStruct.INSTANCE, suppliedPBinding.isSpecial());
			keywordFunctionParametersToBind.put(suppliedPSymbol, functionParameterBinding);
		}

		for (final Iterator<LispStruct> iterator = restList.iterator(); iterator.hasNext(); ) {
			final LispStruct nextArgument = iterator.next();

			if (nextArgument instanceof SymbolStruct) {
				final SymbolStruct<?> keywordArgument = (SymbolStruct) nextArgument;
				if (keysToBindings.containsKey(keywordArgument)) {
					final KeyParameter keyBinding = keysToBindings.remove(keywordArgument);

					if (iterator.hasNext()) {
						final SymbolStruct<?> keySymbol = keyBinding.getVar();
						final LispStruct keyInitForm = iterator.next();
						if (CommonLispSymbols.ALLOW_OTHER_KEYS.equals(nextArgument)) {
							if (!keyInitForm.equals(NullStruct.INSTANCE) && !keyInitForm.equals(NILStruct.INSTANCE)) {
								allowOtherKeys = true;
							}
						}

						FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(keySymbol, keyInitForm, keyBinding.isSpecial());
						keywordFunctionParametersToBind.put(keySymbol, functionParameterBinding);

						final SuppliedPParameter suppliedPBinding = keyBinding.getSuppliedPBinding();
						final SymbolStruct<?> suppliedPSymbol = suppliedPBinding.getVar();
						final LispStruct suppliedPInitForm = TStruct.INSTANCE;

						functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, suppliedPInitForm, suppliedPBinding.isSpecial());
						keywordFunctionParametersToBind.put(suppliedPSymbol, functionParameterBinding);
					} else {
						throw new ProgramErrorException("Expected argument to follow keyword name argument for call to '" + functionClassName + " with key name: " + keywordArgument);
					}
				} else if (CommonLispSymbols.ALLOW_OTHER_KEYS.equals(nextArgument)) {
					final LispStruct allowOtherKeysValue = iterator.next();
					if (!allowOtherKeysValue.equals(NullStruct.INSTANCE) && !allowOtherKeysValue.equals(NILStruct.INSTANCE)) {
						allowOtherKeys = true;
					}
				} else {
					if (iterator.hasNext()) {
						// Consume the next argument
						iterator.next();
					}
					// Check in case the key was supplied twice.
					if (!keys.contains(keywordArgument)) {
						otherKeys.add(keywordArgument);
					}
				}
			} else if (!keysToBindings.isEmpty()) {
				throw new ProgramErrorException("Expected Keyword argument for call to '" + functionClassName + " was: " + nextArgument);
			} else if (restBinding == null) {
				final int numberOfOptionals = optionalBindings.size();
				final int maxNumberProvided = numberOfRequired + numberOfOptionals + numberOfKeys;
				throw new ProgramErrorException("Too many arguments in call to '" + functionClassName + "'. " + numberOfArguments + " arguments provided, at most " + maxNumberProvided + " accepted.");
			}
		}

		if (!allowOtherKeys && !otherKeys.isEmpty() && !keys.isEmpty()) {
			throw new ProgramErrorException("Keyword arguments not found in '" + functionClassName + "' function definition: " + otherKeys);
		}

		if (restBinding != null) {
			final SymbolStruct<?> restSymbol = restBinding.getVar();
			final LispStruct restListStruct = ListStruct.buildProperList(restList);

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(restSymbol, restListStruct, restBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		functionParametersToBind.addAll(keywordFunctionParametersToBind.values());

		for (final AuxParameter auxBinding : auxBindings) {
			final SymbolStruct<?> auxSymbol = auxBinding.getVar();

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(auxSymbol, INIT_FORM_PLACEHOLDER, auxBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		return functionParametersToBind;
	}

	protected WholeParameter getWholeBinding() {
		return null;
	}

	protected EnvironmentParameter getEnvironmentBinding() {
		return null;
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
		macroLambdaListBindings = new MacroLambdaList.Builder().wholeBinding(wholeBinding)
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
	public O expand(final ListStruct form, final Environment environment) {

		// TODO: I don't think the first argument matters right now... This may just be the 'whole' parameter.
		// TODO: The 'environment' to be passed is the 'environment' parameter.
		// TODO: Do we do the '(let ((*environment* environment)) (... expansion-body ...))' here?
		return null;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);
		final ListStruct listStruct = (ListStruct) lispStructs[0];
		final Environment environment = (Environment) lispStructs[1];
		return expand(listStruct, environment);
	}
}
