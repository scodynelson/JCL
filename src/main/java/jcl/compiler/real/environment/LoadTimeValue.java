package jcl.compiler.real.environment;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LoadTimeValue {

	private SymbolStruct<?> name;
	private LispStruct value;

	public LoadTimeValue(final SymbolStruct<?> name, final LispStruct value) {
		this.name = name;
		this.value = value;
	}

	public SymbolStruct<?> getName() {
		return name;
	}

	public void setName(final SymbolStruct<?> name) {
		this.name = name;
	}

	public LispStruct getValue() {
		return value;
	}

	public void setValue(final LispStruct value) {
		this.value = value;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
