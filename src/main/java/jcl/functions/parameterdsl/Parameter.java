package jcl.functions.parameterdsl;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.conditions.exceptions.ProgramErrorException;

public class Parameter {

	private final FunctionParameters functionParameters;
	private final Iterator<LispStruct> iterator;
	private final int numberOfArguments;

	private int numberOfRequired;

	Parameter(final FunctionParameters functionParameters, final List<LispStruct> lispStructs) {
		this.functionParameters = functionParameters;
		iterator = lispStructs.iterator();
		numberOfArguments = lispStructs.size();
	}

	public Parameter requiringAtLeast(final int numberOfRequired) {
		this.numberOfRequired = numberOfRequired;
		return this;
	}

	public Type requiredParameter(final String parameterName) {
		if (!iterator.hasNext()) {
			final String message = String.format("Too few arguments in call to '%s'. %d arguments provided, at least %d required.",
			                                     "functionName",
			                                     numberOfArguments,
			                                     numberOfRequired);
			throw new ProgramErrorException(message);
		}

		final Map<String, LispStruct> requiredParameters = functionParameters.getRequiredParameters();
		return new Type(this, requiredParameters, parameterName, iterator.next());
	}

	public InitialValue optionalParameter(final String parameterName) {
		final Map<String, LispStruct> optionalParameters = functionParameters.getOptionalParameters();
		return new InitialValue(this, optionalParameters, parameterName, iterator);
	}

	public Parameter restParameter() {
		final List<LispStruct> restParameter = functionParameters.getRestParameter();
		while (iterator.hasNext()) {
			restParameter.add(iterator.next());
		}
		return this;
	}

	public InitialValue keyParameter(final String parameterName) {
		final List<LispStruct> restParameter = functionParameters.getRestParameter();
		while (iterator.hasNext()) {
			restParameter.add(iterator.next());
		}


//		final Map<KeywordStruct, LispStruct> optionalParameters = functionParameters.getKeyParameters();
//		return new InitialValue(this, optionalParameters, parameterName, iterator);
		return null;
	}
}
