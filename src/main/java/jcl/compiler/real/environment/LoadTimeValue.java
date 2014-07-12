package jcl.compiler.real.environment;

import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;

import java.util.Map;

public class LoadTimeValue {

	private Map<SymbolStruct, ListStruct> values;

	public LoadTimeValue(final Map<SymbolStruct, ListStruct> values) {
		this.values = values;
	}

	public Map<SymbolStruct, ListStruct> getValues() {
		return values;
	}

	public void setValues(final Map<SymbolStruct, ListStruct> values) {
		this.values = values;
	}
}
