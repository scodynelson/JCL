/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import jcl.compiler.real.environment.binding.ClosureBinding;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class Closure implements Serializable {

	private static final long serialVersionUID = 1822050348694422252L;

	private final List<ClosureBinding> bindings = new ArrayList<>();

	private final int depth;

	public Closure(final int depth) {
		this.depth = depth;
	}

	public List<ClosureBinding> getBindings() {
		return bindings;
	}

	public boolean hasBinding(final SymbolStruct<?> symbolStruct) {
		return bindings.stream()
		               .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public Optional<ClosureBinding> getBinding(final SymbolStruct<?> symbolStruct) {
		return bindings.stream()
		               .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		               .findFirst();
	}

	public void addBinding(final ClosureBinding closureBinding) {
		bindings.add(closureBinding);
	}

	public int getDepth() {
		return depth;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(bindings)
		                            .append(depth)
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
		final Closure rhs = (Closure) obj;
		return new EqualsBuilder().append(bindings, rhs.bindings)
		                          .append(depth, rhs.depth)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(bindings)
		                                                                .append(depth)
		                                                                .toString();
	}
}
