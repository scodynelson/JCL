package jcl.lang.function.parameterdsl;

public class RequiredParameter {

	private final String parameterName;

	RequiredParameter(final String parameterName) {
		this.parameterName = parameterName;
	}

	String getParameterName() {
		return parameterName;
	}
}
