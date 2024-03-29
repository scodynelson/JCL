package jcl.compiler.function.expanders;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.BodyParameter;
import jcl.compiler.environment.binding.lambdalist.DestructuringLambdaList;
import jcl.compiler.environment.binding.lambdalist.EnvironmentParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.compiler.environment.binding.lambdalist.WholeParameter;
import jcl.compiler.function.FunctionParameterBinding;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.statics.CommonLispSymbols;

public abstract class CompiledMacroFunctionExpander<O extends LispStruct> extends MacroFunctionExpander<O> {

	protected Environment environment;

	protected static final LispStruct INIT_FORM_PLACEHOLDER = new LispStruct() {
	};

	protected CompiledMacroFunctionExpander(final Environment environment) {
		this.environment = environment;
	}

	protected CompiledMacroFunctionExpander(final String documentation, final Environment environment) {
		super(documentation);
		this.environment = environment;
	}

	private static final SymbolStruct DUMMY_SYMBOL = SymbolStruct.toLispSymbol("dummySymbol");

	@Override
	public SymbolStruct getFunctionSymbol() {
		// TODO: we can do this better
		return DUMMY_SYMBOL;
	}

	protected List<FunctionParameterBinding> getDestructuringFunctionBindings(final DestructuringLambdaList destructuringLambdaList,
	                                                                          final ListStruct destructuredForm) {
		final WholeParameter wholeBinding = destructuringLambdaList.getWholeBinding();
		final List<RequiredParameter> requiredBindings = destructuringLambdaList.getRequiredBindings();
		final List<OptionalParameter> optionalBindings = destructuringLambdaList.getOptionalBindings();
		final RestParameter restBinding = destructuringLambdaList.getRestBinding();
		final BodyParameter bodyBinding = destructuringLambdaList.getBodyBinding();
		final List<KeyParameter> keyBindings = destructuringLambdaList.getKeyBindings();
		boolean allowOtherKeys = destructuringLambdaList.isAllowOtherKeys();
		final List<AuxParameter> auxBindings = destructuringLambdaList.getAuxBindings();

		final List<FunctionParameterBinding> functionParametersToBind = new ArrayList<>();

		if (wholeBinding != null) {
			final SymbolStruct wholeSymbol = wholeBinding.getVar();
			final LispStruct wholeInitForm = wholeBinding.getInitForm();
			final FunctionParameterBinding wholeParameterBinding = new FunctionParameterBinding(wholeSymbol, wholeInitForm, wholeBinding.isSpecial());
			functionParametersToBind.add(wholeParameterBinding);
		}

		final int numberOfArguments = destructuredForm.length().toJavaInt();
		final Iterator<LispStruct> functionArgumentsIterator = destructuredForm.iterator();

		final String functionClassName = getClass().getName();
		final int numberOfRequired = requiredBindings.size();
		for (final RequiredParameter requiredBinding : requiredBindings) {
			if (!functionArgumentsIterator.hasNext()) {
				throw new ProgramErrorException("Too few arguments in call to '" + functionClassName + "'. " + numberOfArguments + " arguments provided, at least " + numberOfRequired + " required.");
			}

			final SymbolStruct requiredSymbol = requiredBinding.getVar();
			final LispStruct requiredInitForm = functionArgumentsIterator.next();

			final DestructuringLambdaList destructuringForm = requiredBinding.getDestructuringForm();
			if (destructuringForm != null) {
				if (!(requiredInitForm instanceof final ListStruct requiredInitFormList)) {
					throw new ProgramErrorException("Improper destructuring in call to '" + functionClassName + "'.");
				}
				final List<FunctionParameterBinding> destructuringFunctionBindings =
						getDestructuringFunctionBindings(destructuringForm, requiredInitFormList);
				functionParametersToBind.addAll(destructuringFunctionBindings);
			}

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(requiredSymbol, requiredInitForm, requiredBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		for (final OptionalParameter optionalBinding : optionalBindings) {
			final LispStruct optionalInitForm;
			final LispStruct suppliedPInitForm;

			if (functionArgumentsIterator.hasNext()) {
				optionalInitForm = functionArgumentsIterator.next();
				suppliedPInitForm = TStruct.INSTANCE;

				final DestructuringLambdaList destructuringForm = optionalBinding.getDestructuringForm();
				if (destructuringForm != null) {
					if (!(optionalInitForm instanceof final ListStruct optionalInitFormList)) {
						throw new ProgramErrorException("Improper destructuring in call to '" + functionClassName + "'.");
					}
					final List<FunctionParameterBinding> destructuringFunctionBindings =
							getDestructuringFunctionBindings(destructuringForm, optionalInitFormList);
					functionParametersToBind.addAll(destructuringFunctionBindings);
				}
			} else {
				optionalInitForm = INIT_FORM_PLACEHOLDER;
				suppliedPInitForm = NILStruct.INSTANCE;
			}
			final SymbolStruct optionalSymbol = optionalBinding.getVar();

			FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(optionalSymbol, optionalInitForm, optionalBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);

			final Optional<SuppliedPParameter> suppliedPBinding = optionalBinding.getSuppliedPBinding();
			if (suppliedPBinding.isPresent()) {
				final SymbolStruct suppliedPSymbol = suppliedPBinding.get().getVar();

				functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, suppliedPInitForm, suppliedPBinding.get().isSpecial());
				functionParametersToBind.add(functionParameterBinding);
			}
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

			final Optional<SuppliedPParameter> suppliedPBinding = keyBinding.getSuppliedPBinding();
			if (suppliedPBinding.isPresent()) {
				final SymbolStruct suppliedPSymbol = suppliedPBinding.get().getVar();

				functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, NILStruct.INSTANCE, suppliedPBinding.get().isSpecial());
				keywordFunctionParametersToBind.put(suppliedPSymbol, functionParameterBinding);
			}
		}

		for (final Iterator<LispStruct> iterator = restList.iterator(); iterator.hasNext(); ) {
			final LispStruct nextArgument = iterator.next();

			if (nextArgument instanceof final SymbolStruct keywordArgument) {
				if (keysToBindings.containsKey(keywordArgument)) {
					final KeyParameter keyBinding = keysToBindings.remove(keywordArgument);

					if (iterator.hasNext()) {
						final SymbolStruct keySymbol = keyBinding.getVar();
						final LispStruct keyInitForm = iterator.next();
						if (CommonLispSymbols.ALLOW_OTHER_KEYS.eq(nextArgument)) {
							if (!keyInitForm.eq(NILStruct.INSTANCE)) {
								allowOtherKeys = true;
							}
						}

						final DestructuringLambdaList destructuringForm = keyBinding.getDestructuringForm();
						if (destructuringForm != null) {
							if (!(keyInitForm instanceof final ListStruct keyInitFormList)) {
								throw new ProgramErrorException("Improper destructuring in call to '" + functionClassName + "'.");
							}
							final List<FunctionParameterBinding> destructuringFunctionBindings =
									getDestructuringFunctionBindings(destructuringForm, keyInitFormList);
							functionParametersToBind.addAll(destructuringFunctionBindings);
						}

						FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(keySymbol, keyInitForm, keyBinding.isSpecial());
						keywordFunctionParametersToBind.put(keySymbol, functionParameterBinding);

						final Optional<SuppliedPParameter> suppliedPBinding = keyBinding.getSuppliedPBinding();
						if (suppliedPBinding.isPresent()) {
							final SymbolStruct suppliedPSymbol = suppliedPBinding.get().getVar();
							final LispStruct suppliedPInitForm = TStruct.INSTANCE;

							functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, suppliedPInitForm, suppliedPBinding.get().isSpecial());
							keywordFunctionParametersToBind.put(suppliedPSymbol, functionParameterBinding);
						}
					} else {
						throw new ProgramErrorException("Expected argument to follow keyword name argument for call to '" + functionClassName + " with key name: " + keywordArgument);
					}
				} else if (CommonLispSymbols.ALLOW_OTHER_KEYS.eq(nextArgument)) {
					final LispStruct allowOtherKeysValue = iterator.next();
					if (!allowOtherKeysValue.eq(NILStruct.INSTANCE)) {
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
			} else if ((restBinding == null) && (bodyBinding == null)) {
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
			final LispStruct restListStruct = ListStruct.toLispList(restList);

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(restSymbol, restListStruct, restBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		if (bodyBinding != null) {
			final SymbolStruct bodySymbol = bodyBinding.getVar();
			final LispStruct bodyListStruct = ListStruct.toLispList(restList);

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(bodySymbol, bodyListStruct, bodyBinding.isSpecial());
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

		final String functionClassName = getClass().getName();
		final int numberOfRequired = requiredBindings.size();
		for (final RequiredParameter requiredBinding : requiredBindings) {
			if (!functionArgumentsIterator.hasNext()) {
				throw new ProgramErrorException("Too few arguments in call to '" + functionClassName + "'. " + numberOfArguments + " arguments provided, at least " + numberOfRequired + " required.");
			}

			final SymbolStruct requiredSymbol = requiredBinding.getVar();
			final LispStruct requiredInitForm = functionArgumentsIterator.next();

			final DestructuringLambdaList destructuringForm = requiredBinding.getDestructuringForm();
			if (destructuringForm != null) {
				if (!(requiredInitForm instanceof final ListStruct requiredInitFormList)) {
					throw new ProgramErrorException("Improper destructuring in call to '" + functionClassName + "'.");
				}
				final List<FunctionParameterBinding> destructuringFunctionBindings =
						getDestructuringFunctionBindings(destructuringForm, requiredInitFormList);
				functionParametersToBind.addAll(destructuringFunctionBindings);
			}

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(requiredSymbol, requiredInitForm, requiredBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		for (final OptionalParameter optionalBinding : optionalBindings) {
			final LispStruct optionalInitForm;
			final LispStruct suppliedPInitForm;

			if (functionArgumentsIterator.hasNext()) {
				optionalInitForm = functionArgumentsIterator.next();
				suppliedPInitForm = TStruct.INSTANCE;

				final DestructuringLambdaList destructuringForm = optionalBinding.getDestructuringForm();
				if (destructuringForm != null) {
					if (!(optionalInitForm instanceof final ListStruct optionalInitFormList)) {
						throw new ProgramErrorException("Improper destructuring in call to '" + functionClassName + "'.");
					}
					final List<FunctionParameterBinding> destructuringFunctionBindings =
							getDestructuringFunctionBindings(destructuringForm, optionalInitFormList);
					functionParametersToBind.addAll(destructuringFunctionBindings);
				}
			} else {
				optionalInitForm = INIT_FORM_PLACEHOLDER;
				suppliedPInitForm = NILStruct.INSTANCE;
			}
			final SymbolStruct optionalSymbol = optionalBinding.getVar();

			FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(optionalSymbol, optionalInitForm, optionalBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);

			final Optional<SuppliedPParameter> suppliedPBinding = optionalBinding.getSuppliedPBinding();
			if (suppliedPBinding.isPresent()) {
				final SymbolStruct suppliedPSymbol = suppliedPBinding.get().getVar();

				functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, suppliedPInitForm, suppliedPBinding.get().isSpecial());
				functionParametersToBind.add(functionParameterBinding);
			}
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

			final Optional<SuppliedPParameter> suppliedPBinding = keyBinding.getSuppliedPBinding();
			if (suppliedPBinding.isPresent()) {
				final SymbolStruct suppliedPSymbol = suppliedPBinding.get().getVar();

				functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, NILStruct.INSTANCE, suppliedPBinding.get().isSpecial());
				keywordFunctionParametersToBind.put(suppliedPSymbol, functionParameterBinding);
			}
		}

		for (final Iterator<LispStruct> iterator = restList.iterator(); iterator.hasNext(); ) {
			final LispStruct nextArgument = iterator.next();

			if (nextArgument instanceof final SymbolStruct keywordArgument) {
				if (keysToBindings.containsKey(keywordArgument)) {
					final KeyParameter keyBinding = keysToBindings.remove(keywordArgument);

					if (iterator.hasNext()) {
						final SymbolStruct keySymbol = keyBinding.getVar();
						final LispStruct keyInitForm = iterator.next();
						if (CommonLispSymbols.ALLOW_OTHER_KEYS.eq(nextArgument)) {
							if (!keyInitForm.eq(NILStruct.INSTANCE)) {
								allowOtherKeys = true;
							}
						}

						final DestructuringLambdaList destructuringForm = keyBinding.getDestructuringForm();
						if (destructuringForm != null) {
							if (!(keyInitForm instanceof final ListStruct keyInitFormList)) {
								throw new ProgramErrorException("Improper destructuring in call to '" + functionClassName + "'.");
							}
							final List<FunctionParameterBinding> destructuringFunctionBindings =
									getDestructuringFunctionBindings(destructuringForm, keyInitFormList);
							functionParametersToBind.addAll(destructuringFunctionBindings);
						}

						FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(keySymbol, keyInitForm, keyBinding.isSpecial());
						keywordFunctionParametersToBind.put(keySymbol, functionParameterBinding);

						final Optional<SuppliedPParameter> suppliedPBinding = keyBinding.getSuppliedPBinding();
						if (suppliedPBinding.isPresent()) {
							final SymbolStruct suppliedPSymbol = suppliedPBinding.get().getVar();
							final LispStruct suppliedPInitForm = TStruct.INSTANCE;

							functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, suppliedPInitForm, suppliedPBinding.get().isSpecial());
							keywordFunctionParametersToBind.put(suppliedPSymbol, functionParameterBinding);
						}
					} else {
						throw new ProgramErrorException("Expected argument to follow keyword name argument for call to '" + functionClassName + " with key name: " + keywordArgument);
					}
				} else if (CommonLispSymbols.ALLOW_OTHER_KEYS.eq(nextArgument)) {
					final LispStruct allowOtherKeysValue = iterator.next();
					if (!allowOtherKeysValue.eq(NILStruct.INSTANCE)) {
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
			} else if ((restBinding == null) && (bodyBinding == null)) {
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
			final LispStruct restListStruct = ListStruct.toLispList(restList);

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(restSymbol, restListStruct, restBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		if (bodyBinding != null) {
			final SymbolStruct bodySymbol = bodyBinding.getVar();
			final LispStruct bodyListStruct = ListStruct.toLispList(restList);

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(bodySymbol, bodyListStruct, bodyBinding.isSpecial());
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

	protected LispStruct getInitForm(final Environment currentEnvironment, final SymbolStruct parameter) {
		return NILStruct.INSTANCE;
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	@Override
	public O expand(final ListStruct form, final Environment currentEnvironment) {
		macroLambdaListBindings.getWholeBinding().setInitForm(form);
		macroLambdaListBindings.getEnvironmentBinding().setInitForm(currentEnvironment);

		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // MACRO-NAME SYMBOL

		final List<LispStruct> arguments = new ArrayList<>();
		iterator.forEachRemaining(arguments::add);

		final LispStruct[] argsArray = new LispStruct[arguments.size()];
		arguments.toArray(argsArray);

		final List<FunctionParameterBinding> parameterSymbolsToBind = getFunctionBindings(argsArray);
		for (final FunctionParameterBinding parameterSymbolToBind : parameterSymbolsToBind) {
			final SymbolStruct symbol = parameterSymbolToBind.getParameterSymbol();
			LispStruct value = parameterSymbolToBind.getParameterValue();
			if (value instanceof final ValuesStruct valuesStruct) {
				value = valuesStruct.getPrimaryValue();
			} else if (INIT_FORM_PLACEHOLDER.eq(value)) {
				value = getInitForm(environment, symbol);
			}
			final boolean isSpecial = parameterSymbolToBind.isSpecial();
			if (isSpecial) {
				environment.bindDynamicValue(symbol, value);
			} else {
				environment.bindLexicalValue(symbol, value);
			}
		}

		final O result;
		try {
			result = internalApply(environment);
		} catch (final ErrorException ex) {
			throw ex;
		} catch (final Throwable t) {
			throw new ErrorException("Non-Lisp error found.", t);
		} finally {
			for (final FunctionParameterBinding parameterSymbolToUnbind : parameterSymbolsToBind) {
				final SymbolStruct parameterSymbol = parameterSymbolToUnbind.getParameterSymbol();
				final boolean isSpecial = parameterSymbolToUnbind.isSpecial();
				if (isSpecial) {
					environment.unbindDynamicValue(parameterSymbol);
				} else {
					environment.unbindLexicalValue(parameterSymbol);
				}
			}
		}
		return result;
	}

	protected O internalApply(final Environment currentEnvironment) {
		return null;
	}
}
