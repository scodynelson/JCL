package jcl.functions.parameterdsl;

import jcl.LispStruct;
import jcl.symbols.KeywordStruct;
import jcl.symbols.NILStruct;

public class Key {

	private final Parameters parameters;
	private final KeywordStruct keyword;
	private LispStruct initialValue = NILStruct.INSTANCE;
	private Class<? extends LispStruct> clazz;

	Key(final Parameters parameters, final KeywordStruct keyword) {
		this.parameters = parameters;
		this.keyword = keyword;
	}

	public Key withInitialValue(final LispStruct initialValue) {
		this.initialValue = initialValue;
		return this;
	}

	public <T extends LispStruct> Parameters as(final Class<T> clazz) {
		this.clazz = clazz;
		return parameters;
	}

	KeywordStruct getKeyword() {
		return keyword;
	}

	LispStruct getInitialValue() {
		return initialValue;
	}

	Class<? extends LispStruct> getClazz() {
		return clazz;
	}
}
