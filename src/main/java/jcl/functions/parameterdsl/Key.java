package jcl.functions.parameterdsl;

import jcl.LispStruct;
import jcl.symbols.KeywordStruct;
import jcl.symbols.NILStruct;

public class Key {

	private final KeywordStruct keyword;
	private final Parameters parameters;
	private Class<?> clazz;
	private LispStruct initialValue = NILStruct.INSTANCE;

	Key(final KeywordStruct keyword, final Parameters parameters) {
		this.keyword = keyword;
		this.parameters = parameters;
	}

	public <T extends LispStruct> Parameters as(final Class<T> clazz) {
		this.clazz = clazz;
		return parameters;
	}

	public Key withInitialValue(final LispStruct initialValue) {
		this.initialValue = initialValue;
		return this;
	}

	KeywordStruct getKeyword() {
		return keyword;
	}

	Class<?> getClazz() {
		return clazz;
	}
}
