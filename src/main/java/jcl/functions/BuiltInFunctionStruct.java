package jcl.functions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.Parameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import jcl.system.CommonLispSymbols;
import jcl.util.ClassUtils;

public abstract class BuiltInFunctionStruct<P extends FunctionParams> extends FunctionStruct {

	private final String functionName;

	public BuiltInFunctionStruct(final String documentation, final String functionName) {
		super(documentation);
		this.functionName = functionName;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		final OrdinaryLambdaList parsedParameters = fillInParsedParameters(lispStructs);
		final P params = getParams(parsedParameters);
		return internalApply(params);
	}

	protected abstract LispStruct internalApply(P params);

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		initLambdaListBindings();

		final SymbolStruct functionSymbol = getFunctionSymbol();
		functionSymbol.setFunction(this);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		final PackageStruct aPackage = GlobalPackageStruct.COMMON_LISP;
		final SymbolStruct symbol = aPackage.intern(functionName).getSymbol();
		aPackage.export(symbol);
		return symbol;
	}

	private void setParameterValue(final Parameter parameter, final LispStruct value) {
		final Class<? extends LispStruct> valueClass = parameter.getInitFormClass();
		parameter.setInitForm(ClassUtils.convert(valueClass, value));
	}

	protected OrdinaryLambdaList fillInParsedParameters(final LispStruct[] lispStructs) {
		final List<RequiredParameter> requiredBindings = lambdaListBindings.getRequiredBindings();
		final List<OptionalParameter> optionalBindings = lambdaListBindings.getOptionalBindings();
		final RestParameter restBinding = lambdaListBindings.getRestBinding();
		final List<KeyParameter> keyBindings = lambdaListBindings.getKeyBindings();
		boolean allowOtherKeys = lambdaListBindings.isAllowOtherKeys();

		final List<LispStruct> functionArguments = Arrays.asList(lispStructs);
		final int numberOfArguments = functionArguments.size();
		final Iterator<LispStruct> functionArgumentsIterator = functionArguments.iterator();

		final int numberOfRequired = requiredBindings.size();
		for (final RequiredParameter requiredBinding : requiredBindings) {
			if (!functionArgumentsIterator.hasNext()) {
				throw new ProgramErrorException("Too few arguments in call to '" + functionName + "'. " + numberOfArguments + " arguments provided, at least " + numberOfRequired + " required.");
			}

			final LispStruct requiredInitForm = functionArgumentsIterator.next();
			setParameterValue(requiredBinding, requiredInitForm);
		}

		for (final OptionalParameter optionalBinding : optionalBindings) {
			final LispStruct optionalInitForm;

			if (functionArgumentsIterator.hasNext()) {
				optionalInitForm = functionArgumentsIterator.next();
			} else {
				optionalInitForm = optionalBinding.getInitForm();
			}
			setParameterValue(optionalBinding, optionalInitForm);
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

		for (final Iterator<LispStruct> iterator = restList.iterator(); iterator.hasNext(); ) {
			final LispStruct nextArgument = iterator.next();

			if (nextArgument instanceof SymbolStruct) {
				final SymbolStruct keywordArgument = (SymbolStruct) nextArgument;
				if (keysToBindings.containsKey(keywordArgument)) {
					final KeyParameter keyBinding = keysToBindings.remove(keywordArgument);

					if (iterator.hasNext()) {
						final LispStruct keyInitForm = iterator.next();
						if (CommonLispSymbols.ALLOW_OTHER_KEYS.equals(nextArgument)) {
							if (!keyInitForm.equals(NILStruct.INSTANCE)) {
								allowOtherKeys = true;
							}
						}
						setParameterValue(keyBinding, keyInitForm);
					} else {
						throw new ProgramErrorException("Expected argument to follow keyword name argument for call to '" + functionName + " with key name: " + keywordArgument);
					}
				} else if (CommonLispSymbols.ALLOW_OTHER_KEYS.equals(nextArgument)) {
					final LispStruct allowOtherKeysValue = iterator.next();
					if (!allowOtherKeysValue.equals(NILStruct.INSTANCE)) {
						allowOtherKeys = true;
					}
				} else {
					if (iterator.hasNext()) {
						//	 Consume the next argument
						iterator.next();
					}
					// Check in case the key was supplied twice.
					if (!keys.contains(keywordArgument)) {
						otherKeys.add(keywordArgument);
					}
				}
			} else if (!keysToBindings.isEmpty()) {
				throw new ProgramErrorException("Expected Keyword argument for call to '" + functionName + " was: " + nextArgument);
			} else if (restBinding == null) {
				final int numberOfOptionals = optionalBindings.size();
				final int maxNumberProvided = numberOfRequired + numberOfOptionals + numberOfKeys;
				throw new ProgramErrorException("Too many arguments in call to '" + functionName + "'. " + numberOfArguments + " arguments provided, at most " + maxNumberProvided + " accepted.");
			}
		}

		if (!allowOtherKeys && !otherKeys.isEmpty() && !keys.isEmpty()) {
			throw new ProgramErrorException("Keyword arguments not found in '" + functionName + "' function definition: " + otherKeys);
		}

		if (restBinding != null) {
			final LispStruct restListStruct = ListStruct.buildProperList(restList);
			setParameterValue(restBinding, restListStruct);
		}

		return OrdinaryLambdaList.builder()
		                         .requiredBindings(requiredBindings)
		                         .optionalBindings(optionalBindings)
		                         .restBinding(restBinding)
		                         .keyBindings(keyBindings)
		                         .build();
	}

	protected abstract P getParams(OrdinaryLambdaList lambdaList);
}
