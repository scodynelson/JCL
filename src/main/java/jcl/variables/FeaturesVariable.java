package jcl.variables;

import jcl.symbols.SymbolStruct;

import java.util.ArrayList;
import java.util.List;

public class FeaturesVariable implements LispVariable<List<SymbolStruct<?>>> {

	public static final FeaturesVariable INSTANCE = new FeaturesVariable(new ArrayList<>());

	private List<SymbolStruct<?>> value;

	private FeaturesVariable(final List<SymbolStruct<?>> value) {
		this.value = value;
	}

	@Override
	public List<SymbolStruct<?>> getValue() {
		return value;
	}

	public void setValue(final List<SymbolStruct<?>> value) {
		this.value = value;
	}
}
