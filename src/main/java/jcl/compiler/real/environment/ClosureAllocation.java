package jcl.compiler.real.environment;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class ClosureAllocation implements Allocation {

	private final Closure closure;

	public ClosureAllocation(final Closure closure) {
		this.closure = closure;
	}

	public Closure getClosure() {
		return closure;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
