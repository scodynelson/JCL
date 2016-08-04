package jcl.compiler.function;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStructImpl;
import jcl.lang.TStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.NILStruct;
import jcl.type.CompiledFunctionType;
import jcl.type.LispType;

public abstract class CompiledFunctionStruct extends FunctionStruct {

	protected OrdinaryLambdaList lambdaListBindings;

	protected Closure closure;

	protected static final LispStruct INIT_FORM_PLACEHOLDER = new LispStruct() {

		@Override
		public LispType getType() {
			return null;
		}
	};

	protected CompiledFunctionStruct(final Closure closure) {
		this("", closure);
	}

	protected CompiledFunctionStruct(final String documentation, final Closure closure) {
		super(documentation, CompiledFunctionType.INSTANCE);
		this.closure = closure;
	}

	private static final SymbolStructImpl DUMMY_SYMBOL = SymbolStructImpl.valueOf("dummySymbol");

	@Override
	public SymbolStructImpl getFunctionSymbol() {
		// TODO: we can do this better
		return DUMMY_SYMBOL;
	}

	@Override
	@SuppressWarnings({"unchecked", "rawtypes"})
	public LispStruct apply(final LispStruct... lispStructs) {
		final Map<SymbolStructImpl, LispStruct> closureSymbolsToBind = getClosureSymbolBindings();
		for (final Map.Entry<SymbolStructImpl, LispStruct> closureSymbolToBind : closureSymbolsToBind.entrySet()) {
			final SymbolStructImpl symbol = closureSymbolToBind.getKey();
			LispStruct value = closureSymbolToBind.getValue();
			if (value instanceof ValuesStruct) {
				final ValuesStruct valuesStruct = (ValuesStruct) value;
				value = valuesStruct.getPrimaryValue();
			}
			symbol.bindLexicalValue(value);
		}

		final Map<SymbolStructImpl, FunctionStruct> closureFunctionsToBind = getClosureFunctionBindings();
		for (final Map.Entry<SymbolStructImpl, FunctionStruct> closureFunctionToBind : closureFunctionsToBind.entrySet()) {
			final SymbolStructImpl symbol = closureFunctionToBind.getKey();
			final FunctionStruct function = closureFunctionToBind.getValue();
			symbol.bindFunction(function);
		}

		final List<FunctionParameterBinding> parameterSymbolsToBind = getFunctionBindings(lispStructs);
		for (final FunctionParameterBinding parameterSymbolToBind : parameterSymbolsToBind) {
			final SymbolStructImpl symbol = parameterSymbolToBind.getParameterSymbol();
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

		final LispStruct result;
		try {
			result = internalApply(closure);
		} catch (final ErrorException ex) {
			throw ex;
		} catch (final Throwable t) {
			throw new ErrorException("Non-Lisp error found.", t);
		} finally {
			for (final FunctionParameterBinding parameterSymbolToUnbind : parameterSymbolsToBind) {
				final SymbolStructImpl parameterSymbol = parameterSymbolToUnbind.getParameterSymbol();
				final boolean isSpecial = parameterSymbolToUnbind.isSpecial();
				if (isSpecial) {
					parameterSymbol.unbindDynamicValue();
				} else {
					parameterSymbol.unbindLexicalValue();
				}
			}
			for (final SymbolStructImpl closureFunctionToUnbind : closureFunctionsToBind.keySet()) {
				closureFunctionToUnbind.unbindFunction();
			}
			for (final SymbolStructImpl closureSymbolToUnbind : closureSymbolsToBind.keySet()) {
				closureSymbolToUnbind.unbindLexicalValue();
			}
		}
		return result;
	}

	public Map<SymbolStructImpl, LispStruct> getClosureSymbolBindings() {
		if (closure == null) {
			return Collections.emptyMap();
		}
		return closure.getSymbolBindings();
	}

	public Map<SymbolStructImpl, FunctionStruct> getClosureFunctionBindings() {
		if (closure == null) {
			return Collections.emptyMap();
		}
		return closure.getFunctionBindings();
	}

	protected LispStruct internalApply(final Closure currentClosure) {
		return NILStruct.INSTANCE;
	}

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

			final SymbolStructImpl requiredSymbol = requiredBinding.getVar();
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
			final SymbolStructImpl optionalSymbol = optionalBinding.getVar();

			FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(optionalSymbol, optionalInitForm, optionalBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);

			final SuppliedPParameter suppliedPBinding = optionalBinding.getSuppliedPBinding();
			final SymbolStructImpl suppliedPSymbol = suppliedPBinding.getVar();

			functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, suppliedPInitForm, suppliedPBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		final int numberOfKeys = keyBindings.size();
		final Map<SymbolStructImpl, KeyParameter> keysToBindings = new HashMap<>();
		for (final KeyParameter keyBinding : keyBindings) {
			final SymbolStructImpl keyName = keyBinding.getKeyName();
			keysToBindings.put(keyName, keyBinding);
		}

		// Need to wrap the keySet() in a new HashSet because of the remove() operation below on the 'keysToBindings'
		final Set<SymbolStructImpl> keys = new HashSet<>(keysToBindings.keySet());

		final List<LispStruct> restList = new ArrayList<>();

		while (functionArgumentsIterator.hasNext()) {
			final LispStruct nextArgument = functionArgumentsIterator.next();
			restList.add(nextArgument);
		}

		final List<SymbolStructImpl> otherKeys = new ArrayList<>();

		final Map<SymbolStructImpl, FunctionParameterBinding> keywordFunctionParametersToBind = new LinkedHashMap<>(keyBindings.size());
		for (final KeyParameter keyBinding : keyBindings) {
			final SymbolStructImpl keySymbol = keyBinding.getVar();

			FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(keySymbol, INIT_FORM_PLACEHOLDER, keyBinding.isSpecial());
			keywordFunctionParametersToBind.put(keySymbol, functionParameterBinding);

			final SuppliedPParameter suppliedPBinding = keyBinding.getSuppliedPBinding();
			final SymbolStructImpl suppliedPSymbol = suppliedPBinding.getVar();

			functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, NILStruct.INSTANCE, suppliedPBinding.isSpecial());
			keywordFunctionParametersToBind.put(suppliedPSymbol, functionParameterBinding);
		}

		for (final Iterator<LispStruct> iterator = restList.iterator(); iterator.hasNext(); ) {
			final LispStruct nextArgument = iterator.next();

			if (nextArgument instanceof SymbolStructImpl) {
				final SymbolStructImpl keywordArgument = (SymbolStructImpl) nextArgument;
				if (keysToBindings.containsKey(keywordArgument)) {
					final KeyParameter keyBinding = keysToBindings.remove(keywordArgument);

					if (iterator.hasNext()) {
						final SymbolStructImpl keySymbol = keyBinding.getVar();
						final LispStruct keyInitForm = iterator.next();
						if (Parameters.ALLOW_OTHER_KEYS.equals(nextArgument)) {
							if (!keyInitForm.equals(NILStruct.INSTANCE)) {
								allowOtherKeys = true;
							}
						}

						FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(keySymbol, keyInitForm, keyBinding.isSpecial());
						keywordFunctionParametersToBind.put(keySymbol, functionParameterBinding);

						final SuppliedPParameter suppliedPBinding = keyBinding.getSuppliedPBinding();
						final SymbolStructImpl suppliedPSymbol = suppliedPBinding.getVar();
						final LispStruct suppliedPInitForm = TStruct.INSTANCE;

						functionParameterBinding = new FunctionParameterBinding(suppliedPSymbol, suppliedPInitForm, suppliedPBinding.isSpecial());
						keywordFunctionParametersToBind.put(suppliedPSymbol, functionParameterBinding);
					} else {
						throw new ProgramErrorException("Expected argument to follow keyword name argument for call to '" + functionClassName + " with key name: " + keywordArgument);
					}
				} else if (Parameters.ALLOW_OTHER_KEYS.equals(nextArgument)) {
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
			final SymbolStructImpl restSymbol = restBinding.getVar();
			final LispStruct restListStruct = LispStructFactory.toProperList(restList);

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(restSymbol, restListStruct, restBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		functionParametersToBind.addAll(keywordFunctionParametersToBind.values());

		for (final AuxParameter auxBinding : auxBindings) {
			final SymbolStructImpl auxSymbol = auxBinding.getVar();

			final FunctionParameterBinding functionParameterBinding = new FunctionParameterBinding(auxSymbol, INIT_FORM_PLACEHOLDER, auxBinding.isSpecial());
			functionParametersToBind.add(functionParameterBinding);
		}

		return functionParametersToBind;
	}

	protected void initLambdaListBindings() {
		final List<RequiredParameter> requiredBindings = getRequiredBindings();
		final List<OptionalParameter> optionalBindings = getOptionalBindings();
		final RestParameter restBinding = getRestBinding();
		final List<KeyParameter> keyBindings = getKeyBindings();
		final boolean allowOtherKeys = getAllowOtherKeys();
		final List<AuxParameter> auxBindings = getAuxBindings();
		lambdaListBindings = OrdinaryLambdaList.builder()
		                                       .requiredBindings(requiredBindings)
		                                       .optionalBindings(optionalBindings)
		                                       .restBinding(restBinding)
		                                       .keyBindings(keyBindings)
		                                       .allowOtherKeys(allowOtherKeys)
		                                       .auxBindings(auxBindings)
		                                       .build();
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

	protected LispStruct getInitForm(final Closure currentClosure, final SymbolStructImpl parameter) {
		return NILStruct.INSTANCE;
	}
}
