/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

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
import jcl.LispType;
import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.KeywordStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.symbols.TStruct;
import jcl.system.CommonLispSymbols;
import org.apache.commons.collections4.iterators.ArrayIterator;

public abstract class AbstractCommonLispFunctionStruct extends FunctionStruct {

	protected AbstractCommonLispFunctionStruct(final String documentation) {
		super(documentation);
		initLambdaListBindings();
	}

	protected static final LispStruct INIT_FORM_PLACEHOLDER = new LispStruct() {

		@Override
		public LispType getType() {
			return null;
		}
	};

	protected List<FunctionParameterBinding> getFunctionBindings(final LispStruct[] lispStructs) {
		final List<RequiredParameter> requiredBindings = lambdaListBindings.getRequiredBindings();
		final List<OptionalParameter> optionalBindings = lambdaListBindings.getOptionalBindings();
		final RestParameter restBinding = lambdaListBindings.getRestBinding();
		final List<KeyParameter> keyBindings = lambdaListBindings.getKeyBindings();
		boolean allowOtherKeys = lambdaListBindings.isAllowOtherKeys();
		final List<AuxParameter> auxBindings = lambdaListBindings.getAuxBindings();

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

		functionParametersToBind.addAll(keywordFunctionParametersToBind.values());

		for (final AuxParameter auxBinding : auxBindings) {
			final SymbolStruct auxSymbol = auxBinding.getVar();

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(auxSymbol, INIT_FORM_PLACEHOLDER, auxBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		return functionParametersToBind;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);
		return null;
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		final PackageStruct aPackage = GlobalPackageStruct.COMMON_LISP;
		final SymbolStruct symbol = aPackage.intern(functionName()).getSymbol();
		aPackage.export(symbol);
		return symbol;
	}

	protected static Map<KeywordStruct, LispStruct> getKeywords(final LispStruct[] lispStructs, final int keysStart,
	                                                            final KeywordStruct... keywords) {

		final Map<KeywordStruct, LispStruct> keywordMap = new HashMap<>();
		final Iterator<LispStruct> iterator = new ArrayIterator<>(lispStructs, keysStart);

		while (iterator.hasNext()) {
			final LispStruct possibleKeyword = iterator.next();
			if (!(possibleKeyword instanceof KeywordStruct)) {
				break;
			}

			for (final KeywordStruct keyword : keywords) {
				if (keyword.equals(possibleKeyword)) {
					keywordMap.put(keyword, iterator.next());
				}
			}
		}

		return keywordMap;
	}

	protected abstract String functionName();
}
