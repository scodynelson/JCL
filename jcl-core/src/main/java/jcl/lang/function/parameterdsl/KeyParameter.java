package jcl.lang.function.parameterdsl;

import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;

public class KeyParameter {

	private final Parameters parameters;
	private final KeywordStruct keyword;
	private LispStruct initialValue;

	KeyParameter(final Parameters parameters, final KeywordStruct keyword) {
		this.parameters = parameters;
		this.keyword = keyword;
	}

	public Parameters and() {
		return parameters;
	}

	public Parameters withInitialValue(final LispStruct initialValue) {
		this.initialValue = initialValue;
		return parameters;
	}

	KeywordStruct getKeyword() {
		return keyword;
	}

	LispStruct getInitialValue() {
		return initialValue;
	}
}
