package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;

public enum Marker {
	LAMBDA,
	MACROLET,
	LET,
	FLET,
	LABELS;

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this);
	}
}
