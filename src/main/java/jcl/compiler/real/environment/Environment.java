/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import jcl.LispStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public abstract class Environment<E extends Environment<E>> implements LispStruct {

	private static final long serialVersionUID = 7523547599975901124L;

	private final E parent;

	private final List<EnvironmentBinding> bindings = new ArrayList<>();

	private final SymbolTable symbolTable = new SymbolTable();

	protected Environment(final E parent) {
		this.parent = parent;
	}

	public E getParent() {
		return parent;
	}

	public List<EnvironmentBinding> getBindings() {
		return bindings;
	}

	public boolean hasBinding(final SymbolStruct<?> symbolStruct) {
		return bindings.stream()
		               .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public Optional<EnvironmentBinding> getBinding(final SymbolStruct<?> symbolStruct) {
		return bindings.stream()
		               .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		               .findFirst();
	}

	public void addBinding(final EnvironmentBinding environmentBinding) {
		bindings.add(environmentBinding);
	}

	public SymbolTable getSymbolTable() {
		return symbolTable;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
