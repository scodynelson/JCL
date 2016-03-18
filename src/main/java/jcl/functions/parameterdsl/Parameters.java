package jcl.functions.parameterdsl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.symbols.KeywordStruct;

public class Parameters {

	private final FunctionParameters functionParameters;

	private final List<Required> requireds = new ArrayList<>();
	private final List<Optional> optionals = new ArrayList<>();
	private Rest rest;
	private final List<Key> keys = new ArrayList<>();

	private Parameters(final FunctionParameters functionParameters) {
		this.functionParameters = functionParameters;
	}

	public static Parameters forFunction(final String functionName) {
		final FunctionParameters functionParam = new FunctionParameters(functionName);
		return new Parameters(functionParam);
	}

	public Required requiredParameter(final String parameterName) {
		final Required required = new Required(parameterName, this);
		requireds.add(required);
		return required;
	}

	public Optional optionalParameter(final String parameterName) {
		final Optional optional = new Optional(parameterName, this);
		optionals.add(optional);
		return optional;
	}

	public Parameters restParameter() {
		rest = new Rest();
		return this;
	}

	public Key keyParameter(final KeywordStruct keyword) {
		final Key key = new Key(keyword, this);
		keys.add(key);
		return key;
	}

	public FunctionParameters build(final List<LispStruct> lispStructs) {
		final Iterator<LispStruct> iterator = lispStructs.iterator();

		if (iterator.hasNext()) {
			final String message = String.format("Too many arguments in call to '%s'. %d arguments provided, at most %d accepted.",
			                                     functionParameters.getFunctionName(),
			                                     lispStructs.size(),
			                                     requireds.size());
			throw new ProgramErrorException(message);
		}
		return functionParameters;
	}
}
