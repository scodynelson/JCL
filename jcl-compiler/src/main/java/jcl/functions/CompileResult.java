/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import jcl.lang.BooleanStruct;
import jcl.lang.FunctionStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class CompileResult {

	private final FunctionStruct function;

	private final BooleanStruct compiledWithWarnings;

	private final BooleanStruct failedToCompile;

	public CompileResult(final FunctionStruct function, final BooleanStruct compiledWithWarnings, final BooleanStruct failedToCompile) {
		this.function = function;
		this.compiledWithWarnings = compiledWithWarnings;
		this.failedToCompile = failedToCompile;
	}

	public FunctionStruct getFunction() {
		return function;
	}

	public BooleanStruct isCompiledWithWarnings() {
		return compiledWithWarnings;
	}

	public BooleanStruct isFailedToCompile() {
		return failedToCompile;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(function)
		                            .append(compiledWithWarnings)
		                            .append(failedToCompile)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final CompileResult rhs = (CompileResult) obj;
		return new EqualsBuilder().append(compiledWithWarnings, rhs.compiledWithWarnings)
		                          .append(failedToCompile, rhs.failedToCompile)
		                          .append(function, rhs.function)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(function)
		                                                                .append(compiledWithWarnings)
		                                                                .append(failedToCompile)
		                                                                .toString();
	}
}
