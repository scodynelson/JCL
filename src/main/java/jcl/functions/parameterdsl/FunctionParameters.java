package jcl.functions.parameterdsl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jcl.LispStruct;
import jcl.symbols.KeywordStruct;

public final class FunctionParameters {

	private Map<String, LispStruct> requiredParameters;
	private Map<String, LispStruct> optionalParameters;
	private List<LispStruct> restParameter;
	private Map<KeywordStruct, LispStruct> keyParameters;

	public Map<String, LispStruct> getRequiredParameters() {
		if (requiredParameters == null) {
			requiredParameters = new HashMap<>();
		}
		return requiredParameters;
	}

	public Map<String, LispStruct> getOptionalParameters() {
		if (optionalParameters == null) {
			optionalParameters = new HashMap<>();
		}
		return optionalParameters;
	}

	public List<LispStruct> getRestParameter() {
		if (restParameter == null) {
			restParameter = new ArrayList<>();
		}
		return restParameter;
	}

	public Map<KeywordStruct, LispStruct> getKeyParameters() {
		if (keyParameters == null) {
			keyParameters = new HashMap<>();
		}
		return keyParameters;
	}
}
