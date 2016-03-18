package jcl.functions.parameterdsl;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

import jcl.LispStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.symbols.KeywordStruct;
import jcl.symbols.NILStruct;
import jcl.system.CommonLispSymbols;
import jcl.util.ClassUtils;

import static java.util.stream.Collectors.toMap;

public class Parameters {

	private final String functionName;

	private final List<Required> requireds = new ArrayList<>();
	private final List<Optional> optionals = new ArrayList<>();
	private Rest rest;
	private final List<Key> keys = new ArrayList<>();
	private boolean allowOtherKeys;

	private Parameters(final String functionName) {
		this.functionName = functionName;
	}

	public static Parameters forFunction(final String functionName) {
		return new Parameters(functionName);
	}

	public Required requiredParameter(final String parameterName) {
		final Required required = new Required(this, parameterName);
		requireds.add(required);
		return required;
	}

	public Optional optionalParameter(final String parameterName) {
		final Optional optional = new Optional(this, parameterName);
		optionals.add(optional);
		return optional;
	}

	public Parameters restParameter() {
		rest = new Rest();
		return this;
	}

	public Key keyParameter(final KeywordStruct keyword) {
		final Key key = new Key(this, keyword);
		keys.add(key);
		return key;
	}

	public Parameters allowOtherKeys() {
		allowOtherKeys = true;
		return this;
	}

	public FunctionParameters build(final List<LispStruct> arguments) {
		final FunctionParameters functionParameters = new FunctionParameters();

		final int numberOfArguments = arguments.size();
		final int numberOfRequired = requireds.size();
		final int numberOfOptionals = optionals.size();
		final int numberOfKeys = keys.size();

		final Iterator<LispStruct> argumentIterator = arguments.iterator();

		for (final Required required : requireds) {
			if (!argumentIterator.hasNext()) {
				final String message = String.format("Too few arguments in call to '%s'. %d arguments provided, at least %d required.",
				                                     functionName,
				                                     numberOfArguments,
				                                     numberOfRequired);
				throw new ProgramErrorException(message);
			}

			final String parameterName = required.getParameterName();
			final Class<? extends LispStruct> parameterClass = required.getClazz();

			final LispStruct parameterValue = ClassUtils.convert(parameterClass, argumentIterator.next());
			functionParameters.getRequiredParameters().put(parameterName, parameterValue);
		}

		for (final Optional optional : optionals) {
			final String parameterName = optional.getParameterName();
			final Class<? extends LispStruct> parameterClass = optional.getClazz();

			final LispStruct parameterValue;
			if (argumentIterator.hasNext()) {
				parameterValue = ClassUtils.convert(parameterClass, argumentIterator.next());
			} else {
				parameterValue = optional.getInitialValue();
			}
			functionParameters.getOptionalParameters().put(parameterName, parameterValue);
		}

		// Need to wrap the keySet() in a new HashSet because of the remove() operation below on the 'keywordsToKeys'
		final Set<KeywordStruct> consumedKeywords = new HashSet<>();

		final Map<KeywordStruct, Key> keywordsToKeys =
				keys.stream().collect(toMap(Key::getKeyword, Function.identity()));

		final List<LispStruct> restList = new ArrayList<>();
		argumentIterator.forEachRemaining(restList::add);

		if (rest != null) {
			functionParameters.getRestParameter().addAll(restList);
		}

		final List<KeywordStruct> otherKeywords = new ArrayList<>();
		boolean doNotAllowOtherKeys = !allowOtherKeys;

		for (final Iterator<LispStruct> restIterator = restList.iterator(); restIterator.hasNext(); ) {
			final LispStruct argument = restIterator.next();

			if (argument instanceof KeywordStruct) {
				final KeywordStruct keywordArgument = (KeywordStruct) argument;

				if (keywordsToKeys.containsKey(keywordArgument)) {
					final Key key = keywordsToKeys.remove(keywordArgument);
					consumedKeywords.add(keywordArgument);

					if (restIterator.hasNext()) {
						final LispStruct parameterValue = restIterator.next();

						if (CommonLispSymbols.ALLOW_OTHER_KEYS.equals(argument)) {
							if (!parameterValue.equals(NILStruct.INSTANCE)) {
								doNotAllowOtherKeys = false;
							}
						}

						final KeywordStruct keyword = key.getKeyword();
						final Class<? extends LispStruct> parameterClass = key.getClazz();
						functionParameters.getKeyParameters().put(keyword, ClassUtils.convert(parameterClass, parameterValue));
					} else {
						final String message = String.format("Expected argument to follow keyword name argument for call to '%s' with key name: %s.",
						                                     functionName,
						                                     keywordArgument);
						throw new ProgramErrorException(message);
					}
				} else if (CommonLispSymbols.ALLOW_OTHER_KEYS.equals(argument)) {
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
					if (!consumedKeywords.contains(keywordArgument)) {
						otherKeywords.add(keywordArgument);
					}
				}
			} else if (!keywordsToKeys.isEmpty()) {
				final String message = String.format("Expected Keyword argument for call to '%s', but was: %s.",
				                                     functionName,
				                                     argument);
				throw new ProgramErrorException(message);
			} else if (rest == null) {
				final int maxNumberProvided = numberOfRequired + numberOfOptionals + numberOfKeys;
				final String message = String.format("Too many arguments in call to '%s'. %d arguments provided, at most %d accepted.",
				                                     functionName,
				                                     numberOfArguments,
				                                     maxNumberProvided);
				throw new ProgramErrorException(message);
			}
		}

		if (doNotAllowOtherKeys && !otherKeywords.isEmpty() && !consumedKeywords.isEmpty()) {
			final String message = String.format("Keyword arguments not found in '%s' function definition: %s.",
			                                     functionName,
			                                     otherKeywords);
			throw new ProgramErrorException(message);
		}

		for (final Key key : keywordsToKeys.values()) {
			final KeywordStruct keyword = key.getKeyword();
			final LispStruct parameterValue = key.getInitialValue();
			functionParameters.getKeyParameters().put(keyword, parameterValue);
		}

		return functionParameters;
	}
}
