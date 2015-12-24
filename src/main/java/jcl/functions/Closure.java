/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions;

import java.util.LinkedHashMap;
import java.util.Map;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;

public class Closure {

	private final Closure parent;

	private final Map<SymbolStruct, LispStruct> symbolBindings = new LinkedHashMap<>();

	private final Map<SymbolStruct, FunctionStruct> functionBindings = new LinkedHashMap<>();

	public Closure(final Closure parent) {
		this.parent = parent;

		if (parent != null) {
			symbolBindings.putAll(parent.symbolBindings);
			functionBindings.putAll(parent.functionBindings);
		}
	}

	public Closure getParent() {
		return parent;
	}

	public Map<SymbolStruct, LispStruct> getSymbolBindings() {
		return symbolBindings;
	}

	public Map<SymbolStruct, FunctionStruct> getFunctionBindings() {
		return functionBindings;
	}

//	@Override
//	public int hashCode() {
//		return new HashCodeBuilder().append(parent)
//		                            .append(symbolBindings)
//		                            .append(functionBindings)
//		                            .toHashCode();
//	}
//
//	@Override
//	public boolean equals(final Object obj) {
//		if (obj == null) {
//			return false;
//		}
//		if (obj == this) {
//			return true;
//		}
//		if (obj.getClass() != getClass()) {
//			return false;
//		}
//		final Closure rhs = (Closure) obj;
//		return new EqualsBuilder().append(parent, rhs.parent)
//		                          .append(symbolBindings, rhs.symbolBindings)
//		                          .append(functionBindings, rhs.functionBindings)
//		                          .isEquals();
//	}
//
//	@Override
//	public String toString() {
//		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(parent)
//		                                                                .append(symbolBindings)
//		                                                                .append(functionBindings)
//		                                                                .toString();
//	}
}
