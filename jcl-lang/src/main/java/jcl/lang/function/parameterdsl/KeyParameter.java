package jcl.lang.function.parameterdsl;

import jcl.lang.KeywordStructImpl;
import jcl.lang.LispStruct;

public class KeyParameter {

	private final Parameters parameters;
	private final KeywordStructImpl keyword;
	private LispStruct initialValue;

	KeyParameter(final Parameters parameters, final KeywordStructImpl keyword) {
		this.parameters = parameters;
		this.keyword = keyword;
	}

	public Parameters withInitialValue(final LispStruct initialValue) {
		this.initialValue = initialValue;
		return parameters;
	}

	KeywordStructImpl getKeyword() {
		return keyword;
	}

	LispStruct getInitialValue() {
		return initialValue;
	}
}
