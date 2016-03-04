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
import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.BodyParameter;
import jcl.compiler.environment.binding.lambdalist.EnvironmentParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.MacroLambdaList;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.compiler.environment.binding.lambdalist.WholeParameter;
import jcl.compiler.struct.ValuesStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.Closure;
import jcl.functions.FunctionParameterBinding;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.system.CommonLispSymbols;

public abstract class MacroFunctionExpander<O extends LispStruct> extends MacroExpander<O, ListStruct> {

	protected MacroLambdaList macroLambdaListBindings;

	protected MacroFunctionExpander() {
		this(null);
	}

	protected MacroFunctionExpander(final Closure closure) {
		super(closure);
	}

	protected MacroFunctionExpander(final String documentation, final Closure closure) {
		super(documentation, closure);
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		final SymbolStruct functionSymbol = getFunctionSymbol();
		functionSymbol.setMacroFunctionExpander(this);
	}

	@Override
	protected List<FunctionParameterBinding> getFunctionBindings(final LispStruct[] lispStructs) {
		final WholeParameter wholeBinding = macroLambdaListBindings.getWholeBinding();
		final EnvironmentParameter environmentBinding = macroLambdaListBindings.getEnvironmentBinding();
		final List<RequiredParameter> requiredBindings = macroLambdaListBindings.getRequiredBindings();
		final List<OptionalParameter> optionalBindings = macroLambdaListBindings.getOptionalBindings();
		final RestParameter restBinding = macroLambdaListBindings.getRestBinding();
		final BodyParameter bodyBinding = macroLambdaListBindings.getBodyBinding();
		final List<KeyParameter> keyBindings = macroLambdaListBindings.getKeyBindings();
		boolean allowOtherKeys = macroLambdaListBindings.isAllowOtherKeys();
		final List<AuxParameter> auxBindings = macroLambdaListBindings.getAuxBindings();

		final List<FunctionParameterBinding> functionParametersToBind = new ArrayList<>();

		final SymbolStruct wholeSymbol = wholeBinding.getVar();
		final LispStruct wholeInitForm = wholeBinding.getInitForm();
		final FunctionParameterBinding wholeParameterBinding = new FunctionParameterBinding(wholeSymbol, wholeInitForm, wholeBinding.isSpecial());
		functionParametersToBind.add(wholeParameterBinding);

		final SymbolStruct environmentSymbol = environmentBinding.getVar();
		final LispStruct environmentInitForm = environmentBinding.getInitForm();
		final FunctionParameterBinding environmentParameterBinding = new FunctionParameterBinding(environmentSymbol, environmentInitForm, environmentBinding.isSpecial());
		functionParametersToBind.add(environmentParameterBinding);

		final List<LispStruct> functionArguments = Arrays.asList(lispStructs);
		final int numberOfArguments = functionArguments.size();
		final Iterator<LispStruct> functionArgumentsIterator = functionArguments.iterator();

		final String functionClassName = getClass().getSimpleName();
		final int numberOfRequired = requiredBindings.size();
		for (final RequiredParameter requiredBinding : requiredBindings) {
			if (!functionArgumentsIterator.hasNext()) {
				throw new ProgramErrorException("Too few arguments in call to '" + functionClassName + "'. " + numberOfArguments + " arguments provided, at least " + numberOfRequired + " required.");
			}

			final SymbolStruct requiredSymbol = requiredBinding.getVar();
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
			final SymbolStruct optionalSymbol = optionalBinding.getVar();

			FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(optionalSymbol, optionalInitForm, optionalBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);

			final SuppliedPParameter suppliedPBinding = optionalBinding.getSuppliedPBinding();
			final SymbolStruct suppliedPSymbol = suppliedPBinding.getVar();

			functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, suppliedPInitForm, suppliedPBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		final int numberOfKeys = keyBindings.size();
		final Map<SymbolStruct, KeyParameter> keysToBindings = new HashMap<>();
		for (final KeyParameter keyBinding : keyBindings) {
			final SymbolStruct keyName = keyBinding.getKeyName();
			keysToBindings.put(keyName, keyBinding);
		}

		// Need to wrap the keySet() in a new HashSet because of the remove() operation below on the 'keysToBindings'
		final Set<SymbolStruct> keys = new HashSet<>(keysToBindings.keySet());

		final List<LispStruct> restList = new ArrayList<>();

		while (functionArgumentsIterator.hasNext()) {
			final LispStruct nextArgument = functionArgumentsIterator.next();
			restList.add(nextArgument);
		}

		final List<SymbolStruct> otherKeys = new ArrayList<>();

		final Map<SymbolStruct, FunctionParameterBinding> keywordFunctionParametersToBind = new LinkedHashMap<>(keyBindings.size());
		for (final KeyParameter keyBinding : keyBindings) {
			final SymbolStruct keySymbol = keyBinding.getVar();

			FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(keySymbol, INIT_FORM_PLACEHOLDER, keyBinding.isSpecial());
			keywordFunctionParametersToBind.put(keySymbol, functionParameterBinding);

			final SuppliedPParameter suppliedPBinding = keyBinding.getSuppliedPBinding();
			final SymbolStruct suppliedPSymbol = suppliedPBinding.getVar();

			functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, NILStruct.INSTANCE, suppliedPBinding.isSpecial());
			keywordFunctionParametersToBind.put(suppliedPSymbol, functionParameterBinding);
		}

		for (final Iterator<LispStruct> iterator = restList.iterator(); iterator.hasNext(); ) {
			final LispStruct nextArgument = iterator.next();

			if (nextArgument instanceof SymbolStruct) {
				final SymbolStruct keywordArgument = (SymbolStruct) nextArgument;
				if (keysToBindings.containsKey(keywordArgument)) {
					final KeyParameter keyBinding = keysToBindings.remove(keywordArgument);

					if (iterator.hasNext()) {
						final SymbolStruct keySymbol = keyBinding.getVar();
						final LispStruct keyInitForm = iterator.next();
						if (CommonLispSymbols.ALLOW_OTHER_KEYS.equals(nextArgument)) {
							if (!keyInitForm.equals(NILStruct.INSTANCE)) {
								allowOtherKeys = true;
							}
						}

						FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(keySymbol, keyInitForm, keyBinding.isSpecial());
						keywordFunctionParametersToBind.put(keySymbol, functionParameterBinding);

						final SuppliedPParameter suppliedPBinding = keyBinding.getSuppliedPBinding();
						final SymbolStruct suppliedPSymbol = suppliedPBinding.getVar();
						final LispStruct suppliedPInitForm = TStruct.INSTANCE;

						functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, suppliedPInitForm, suppliedPBinding.isSpecial());
						keywordFunctionParametersToBind.put(suppliedPSymbol, functionParameterBinding);
					} else {
						throw new ProgramErrorException("Expected argument to follow keyword name argument for call to '" + functionClassName + " with key name: " + keywordArgument);
					}
				} else if (CommonLispSymbols.ALLOW_OTHER_KEYS.equals(nextArgument)) {
					final LispStruct allowOtherKeysValue = iterator.next();
					if (!allowOtherKeysValue.equals(NILStruct.INSTANCE)) {
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
			final SymbolStruct restSymbol = restBinding.getVar();
			final LispStruct restListStruct = ListStruct.buildProperList(restList);

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(restSymbol, restListStruct, restBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		if (bodyBinding != null) {
			final SymbolStruct bodySymbol = bodyBinding.getVar();
			final LispStruct bodListStruct = ListStruct.buildProperList(restList);

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(bodySymbol, bodListStruct, bodyBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		functionParametersToBind.addAll(keywordFunctionParametersToBind.values());

		for (final AuxParameter auxBinding : auxBindings) {
			final SymbolStruct auxSymbol = auxBinding.getVar();

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(auxSymbol, INIT_FORM_PLACEHOLDER, auxBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		return functionParametersToBind;
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

	@SuppressWarnings({"unchecked", "rawtypes"})
	@Override
	public O expand(final ListStruct form, final Environment environment) {
		macroLambdaListBindings.getWholeBinding().setInitForm(form);
		macroLambdaListBindings.getEnvironmentBinding().setInitForm(environment);

		final ListStruct arguments = form.getRest();
		final List<LispStruct> asJavaList = arguments.getAsJavaList();

		final LispStruct[] argsArray = new LispStruct[asJavaList.size()];
		asJavaList.toArray(argsArray);

		final Map<SymbolStruct, LispStruct> closureSymbolsToBind = getClosureSymbolBindings();
		for (final Map.Entry<SymbolStruct, LispStruct> closureSymbolToBind : closureSymbolsToBind.entrySet()) {
			final SymbolStruct symbol = closureSymbolToBind.getKey();
			LispStruct value = closureSymbolToBind.getValue();
			if (value instanceof ValuesStruct) {
				final ValuesStruct valuesStruct = (ValuesStruct) value;
				value = valuesStruct.getPrimaryValue();
			}
			symbol.bindLexicalValue(value);
		}

		final Map<SymbolStruct, FunctionStruct> closureFunctionsToBind = getClosureFunctionBindings();
		for (final Map.Entry<SymbolStruct, FunctionStruct> closureFunctionToBind : closureFunctionsToBind.entrySet()) {
			final SymbolStruct symbol = closureFunctionToBind.getKey();
			final FunctionStruct function = closureFunctionToBind.getValue();
			symbol.bindFunction(function);
		}

		final List<FunctionParameterBinding> parameterSymbolsToBind = getFunctionBindings(argsArray);
		for (final FunctionParameterBinding parameterSymbolToBind : parameterSymbolsToBind) {
			final SymbolStruct symbol = parameterSymbolToBind.getParameterSymbol();
			LispStruct value = parameterSymbolToBind.getParameterValue();
			if (value instanceof ValuesStruct) {
				final ValuesStruct valuesStruct = (ValuesStruct) value;
				value = valuesStruct.getPrimaryValue();
			} else if (INIT_FORM_PLACEHOLDER.equals(value)) {
				value = getInitForm(closure, symbol);
			}
			final boolean isSpecial = parameterSymbolToBind.isSpecial();
			if (isSpecial) {
				symbol.bindDynamicValue(value);
			} else {
				symbol.bindLexicalValue(value);
			}
		}

		final O result;
		try {
			result = internalApply(closure);
		} catch (final ErrorException ex) {
			throw ex;
		} catch (final Throwable t) {
			throw new ErrorException("Non-Lisp error found.", t);
		} finally {
			for (final FunctionParameterBinding parameterSymbolToUnbind : parameterSymbolsToBind) {
				final SymbolStruct parameterSymbol = parameterSymbolToUnbind.getParameterSymbol();
				final boolean isSpecial = parameterSymbolToUnbind.isSpecial();
				if (isSpecial) {
					parameterSymbol.unbindDynamicValue();
				} else {
					parameterSymbol.unbindLexicalValue();
				}
			}
			for (final SymbolStruct closureFunctionToUnbind : closureFunctionsToBind.keySet()) {
				closureFunctionToUnbind.unbindFunction();
			}
			for (final SymbolStruct closureSymbolToUnbind : closureSymbolsToBind.keySet()) {
				closureSymbolToUnbind.unbindLexicalValue();
			}
		}
		return result;
	}

	@Override
	protected O internalApply(final Closure currentClosure) {
		return null;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		final ListStruct listStruct = (ListStruct) lispStructs[0];
		final Environment environment = (Environment) lispStructs[1];
		return expand(listStruct, environment);
	}
}
