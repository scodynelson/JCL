package jcl.lang.function.parameterdsl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.NILStruct;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.iterators.ObjectArrayIterator;

import static java.util.stream.Collectors.toMap;

public final class Parameters {

	public static final KeywordStruct ALLOW_OTHER_KEYS = KeywordStruct.valueOf("ALLOW-OTHER-KEYS");

	private final String functionName;

	private List<RequiredParameter> requiredParameters;
	private List<OptionalParameter> optionalParameters;
	private Object restPlaceholder;
	private List<KeyParameter> keyParameters;
	private boolean allowOtherKeys;

	private Parameters(final String functionName) {
		this.functionName = functionName;
	}

	public static Parameters forFunction(final String functionName) {
		return new Parameters(functionName);
	}

	public Parameters requiredParameter(final String parameterName) {
		final RequiredParameter requiredParameter = new RequiredParameter(parameterName);
		if (requiredParameters == null) {
			requiredParameters = new ArrayList<>();
		}
		requiredParameters.add(requiredParameter);
		return this;
	}

	public OptionalParameter optionalParameter(final String parameterName) {
		final OptionalParameter optionalParameter = new OptionalParameter(this, parameterName);
		if (optionalParameters == null) {
			optionalParameters = new ArrayList<>();
		}
		optionalParameters.add(optionalParameter);
		return optionalParameter;
	}

	public Parameters restParameter() {
		restPlaceholder = new Object();
		return this;
	}

	public KeyParameter keyParameter(final KeywordStruct keyword) {
		final KeyParameter keyParameter = new KeyParameter(this, keyword);
		if (keyParameters == null) {
			keyParameters = new ArrayList<>();
		}
		keyParameters.add(keyParameter);
		return keyParameter;
	}

	public Parameters allowOtherKeys() {
		allowOtherKeys = true;
		return this;
	}

	public Arguments build(final LispStruct... arguments) {
		final int numberOfArguments = arguments.length;
		final Arguments resultArguments = new Arguments();

		final Iterator<LispStruct> argumentIterator = new ObjectArrayIterator<>(arguments);

		final int numberOfRequired = CollectionUtils.size(requiredParameters);
		final int numberOfOptionals = CollectionUtils.size(optionalParameters);
		final int numberOfKeys = CollectionUtils.size(keyParameters);

		if (requiredParameters != null) {
			for (final RequiredParameter requiredParameter : requiredParameters) {
				if (!argumentIterator.hasNext()) {
					final String message = String.format("Too few arguments in call to '%s'. %d arguments provided, at least %d required.",
					                                     functionName,
					                                     numberOfArguments,
					                                     numberOfRequired);
					throw new ProgramErrorException(message);
				}

				final String parameterName = requiredParameter.getParameterName();
				final LispStruct parameterValue = argumentIterator.next();
				resultArguments.getRequiredParameters().put(parameterName, parameterValue);
			}
		}

		if (optionalParameters != null) {
			for (final OptionalParameter optionalParameter : optionalParameters) {
				final String parameterName = optionalParameter.getParameterName();

				final LispStruct parameterValue;
				if (argumentIterator.hasNext()) {
					parameterValue = argumentIterator.next();
				} else {
					parameterValue = optionalParameter.getInitialValue();
				}
				resultArguments.getOptionalParameters().put(parameterName, parameterValue);
			}
		}

		Map<KeywordStruct, KeyParameter> keywordsToKeys = Collections.emptyMap();
		if (keyParameters != null) {
			keywordsToKeys = keyParameters.stream().collect(toMap(KeyParameter::getKeyword, Function.identity()));
		}

		List<LispStruct> restList = Collections.emptyList();
		if (argumentIterator.hasNext()) {
			restList = new ArrayList<>();
			argumentIterator.forEachRemaining(restList::add);
		}

		if (restPlaceholder != null) {
			resultArguments.getRestParameter().addAll(restList);
		}

		List<KeywordStruct> otherKeywords = null;
		Set<KeywordStruct> consumedKeywords = null;

		boolean doNotAllowOtherKeys = !allowOtherKeys;

		for (final Iterator<LispStruct> restIterator = restList.iterator(); restIterator.hasNext(); ) {
			final LispStruct argument = restIterator.next();

			if (argument instanceof KeywordStruct) {
				final KeywordStruct keywordArgument = (KeywordStruct) argument;

				if (keywordsToKeys.containsKey(keywordArgument)) {
					final KeyParameter keyParameter = keywordsToKeys.remove(keywordArgument);
					if (consumedKeywords == null) {
						consumedKeywords = new HashSet<>();
					}
					consumedKeywords.add(keywordArgument);

					if (restIterator.hasNext()) {
						final LispStruct parameterValue = restIterator.next();

						if (ALLOW_OTHER_KEYS.equals(argument)) {
							if (!NILStruct.INSTANCE.equals(parameterValue)) {
								doNotAllowOtherKeys = false;
							}
						}

						final KeywordStruct keyword = keyParameter.getKeyword();
						resultArguments.getKeyParameters().put(keyword, parameterValue);
					} else {
						final String message = String.format("Expected argument to follow keyword name argument for call to '%s' with key name: %s.",
						                                     functionName,
						                                     keywordArgument);
						throw new ProgramErrorException(message);
					}
				} else if (ALLOW_OTHER_KEYS.equals(argument)) {
					final LispStruct allowOtherKeysValue = restIterator.next();
					if (!NILStruct.INSTANCE.equals(allowOtherKeysValue)) {
						doNotAllowOtherKeys = false;
					}
				} else {
					if (restIterator.hasNext()) {
						// Consume the next argument.
						restIterator.next();
					}
					// Check in case the key was supplied twice. If it's a duplicate key, just ignore it.
					if (consumedKeywords == null) {
						continue;
					}
					if (!consumedKeywords.contains(keywordArgument)) {
						if (otherKeywords == null) {
							otherKeywords = new ArrayList<>();
						}
						otherKeywords.add(keywordArgument);
					}
				}
			} else if (!keywordsToKeys.isEmpty()) {
				final String message = String.format("Expected Keyword argument for call to '%s', but was: %s.",
				                                     functionName,
				                                     argument);
				throw new ProgramErrorException(message);
			} else if (restPlaceholder == null) {
				final int maxNumberProvided = numberOfRequired + numberOfOptionals + numberOfKeys;
				final String message = String.format("Too many arguments in call to '%s'. %d arguments provided, at most %d accepted.",
				                                     functionName,
				                                     numberOfArguments,
				                                     maxNumberProvided);
				throw new ProgramErrorException(message);
			}
		}

		if (doNotAllowOtherKeys && CollectionUtils.isNotEmpty(otherKeywords) && CollectionUtils.isNotEmpty(consumedKeywords)) {
			final String message = String.format("Keyword arguments not found in '%s' function definition: %s.",
			                                     functionName,
			                                     otherKeywords);
			throw new ProgramErrorException(message);
		}

		for (final KeyParameter keyParameter : keywordsToKeys.values()) {
			final KeywordStruct keyword = keyParameter.getKeyword();
			final LispStruct parameterValue = keyParameter.getInitialValue();
			resultArguments.getKeyParameters().put(keyword, parameterValue);
		}

		return resultArguments;
	}
}
