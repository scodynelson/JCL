package jcl.compiler.real.environment;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class LoadTimeValue {

	private final String name;
	private final LispStruct value;

	public LoadTimeValue(final String name, final LispStruct value) {
		this.name = name;
		this.value = value;
	}

	public String getName() {
		return name;
	}

	public LispStruct getValue() {
		return value;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
