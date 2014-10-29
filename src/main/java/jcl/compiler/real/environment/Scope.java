package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;

public enum Scope {
	LEXICAL,
	DYNAMIC;

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this);
	}
}
