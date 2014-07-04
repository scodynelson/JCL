package jcl.variables;

import jcl.LispStruct;

import java.util.ArrayList;
import java.util.List;

public class FeaturesVariable implements LispVariable<List<LispStruct>> {

	public static final FeaturesVariable INSTANCE = new FeaturesVariable(new ArrayList<>());

	private List<LispStruct> value;

	private FeaturesVariable(final List<LispStruct> value) {
		this.value = value;
	}

	@Override
	public List<LispStruct> getValue() {
		return value;
	}

	public void setValue(final List<LispStruct> value) {
		this.value = value;
	}
}
