package jcl.variables;

import jcl.structs.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class FeaturesVariable implements LispVariable<List<SymbolStruct<?>>> {

	public static final FeaturesVariable INSTANCE = new FeaturesVariable(new ArrayList<SymbolStruct<?>>());

	private List<SymbolStruct<?>> value;

	private FeaturesVariable(final List<SymbolStruct<?>> value) {
		this.value = value;
	}

	public List<SymbolStruct<?>> getValue() {
		return value;
	}

	public void setValue(final List<SymbolStruct<?>> value) {
		this.value = value;
	}
}
