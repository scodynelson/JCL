/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.environment.binding.ClosureBinding;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

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

	public boolean hasBinding(final SymbolElement symbolStruct) {
		return bindings.stream()
		               .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public Optional<ClosureBinding> getBinding(final SymbolElement symbolStruct) {
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
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
