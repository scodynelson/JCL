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

import jcl.compiler.environment.Environment;
import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.compiler.environment.binding.lambdalist.SuppliedPParameter;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.function.FunctionStructImpl;
import jcl.lang.statics.CommonLispSymbols;

/*
NOTE: This could also be called a "Closure"
 */
public abstract class CompiledFunctionStruct extends FunctionStructImpl {

	protected OrdinaryLambdaList lambdaListBindings;

	protected Environment environment;

	protected static final LispStruct INIT_FORM_PLACEHOLDER = new LispStruct() {
	};

	protected CompiledFunctionStruct(final Environment environment) {
		this.environment = environment;
	}

	protected CompiledFunctionStruct(final String documentation, final Environment environment) {
		super(documentation);
		this.environment = environment;
	}

	private static final SymbolStruct DUMMY_SYMBOL = SymbolStruct.toLispSymbol("dummySymbol");

	@Override
	public SymbolStruct getFunctionSymbol() {
		// TODO: we can do this better
		return DUMMY_SYMBOL;
	}

	@Override
	@SuppressWarnings({"unchecked", "rawtypes"})
	public LispStruct apply(final LispStruct... lispStructs) {
		final List<FunctionParameterBinding> parameterSymbolsToBind = getFunctionBindings(lispStructs);
		for (final FunctionParameterBinding parameterSymbolToBind : parameterSymbolsToBind) {
			final SymbolStruct symbol = parameterSymbolToBind.getParameterSymbol();
			LispStruct value = parameterSymbolToBind.getParameterValue();
			if (value instanceof ValuesStruct) {
				final ValuesStruct valuesStruct = (ValuesStruct) value;
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

		final LispStruct result;
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

	protected LispStruct internalApply(final Environment currentEnvironment) {
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
						if (CommonLispSymbols.ALLOW_OTHER_KEYS.eq(nextArgument)) {
							if (!keyInitForm.eq(NILStruct.INSTANCE)) {
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
			final LispStruct restListStruct = ListStruct.toLispList(restList);

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

	protected void initLambdaListBindings(final Environment currentEnvironment) {
		final List<RequiredParameter> requiredBindings = getRequiredBindings(currentEnvironment);
		final List<OptionalParameter> optionalBindings = getOptionalBindings(currentEnvironment);
		final RestParameter restBinding = getRestBinding(currentEnvironment);
		final List<KeyParameter> keyBindings = getKeyBindings(currentEnvironment);
		final boolean allowOtherKeys = getAllowOtherKeys(currentEnvironment);
		final List<AuxParameter> auxBindings = getAuxBindings(currentEnvironment);
		lambdaListBindings = OrdinaryLambdaList.builder()
		                                       .requiredBindings(requiredBindings)
		                                       .optionalBindings(optionalBindings)
		                                       .restBinding(restBinding)
		                                       .keyBindings(keyBindings)
		                                       .allowOtherKeys(allowOtherKeys)
		                                       .auxBindings(auxBindings)
		                                       .build();
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

	protected LispStruct getInitForm(final Environment currentEnvironment, final SymbolStruct parameter) {
		return NILStruct.INSTANCE;
	}

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.COMPILED_FUNCTION;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.COMPILED_FUNCTION) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
