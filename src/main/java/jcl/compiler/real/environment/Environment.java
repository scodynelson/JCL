/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.EnvironmentBinding;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class Environment implements LispStruct {

	public static final Environment NULL = new LambdaEnvironment(null, 0);

	private static final long serialVersionUID = 7523547599975901124L;

	private final Environment parent;

	private final List<EnvironmentParameterBinding> lexicalBindings = new ArrayList<>();

	private final List<EnvironmentBinding<?>> dynamicBindings = new ArrayList<>();

	private final SymbolTable symbolTable = new SymbolTable();

	private final Closure closure;

	private final List<LoadTimeValue> loadTimeValues = new ArrayList<>();

	protected Environment(final Environment parent, final int closureDepth) {
		this.parent = parent;
		closure = new Closure(closureDepth);
	}

	public Environment getParent() {
		return parent;
	}

	public List<EnvironmentParameterBinding> getLexicalBindings() {
		return lexicalBindings;
	}

	public boolean hasLexicalBinding(final SymbolStruct<?> symbolStruct) {
		return lexicalBindings.stream()
		                      .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public Optional<EnvironmentParameterBinding> getLexicalBinding(final SymbolStruct<?> symbolStruct) {
		return lexicalBindings.stream()
		                      .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		                      .findFirst();
	}

	public void addLexicalBinding(final EnvironmentParameterBinding environmentBinding) {
		lexicalBindings.add(environmentBinding);
	}

	public List<EnvironmentBinding<?>> getDynamicBindings() {
		return dynamicBindings;
	}

	public boolean hasDynamicBinding(final SymbolStruct<?> symbolStruct) {
		return dynamicBindings.stream()
		                      .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public Optional<EnvironmentBinding<?>> getDynamicBinding(final SymbolStruct<?> symbolStruct) {
		return dynamicBindings.stream()
		                      .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		                      .findFirst();
	}

	public void addDynamicBinding(final EnvironmentBinding<?> environmentBinding) {
		dynamicBindings.add(environmentBinding);
	}

	public SymbolTable getSymbolTable() {
		return symbolTable;
	}

	public Closure getClosure() {
		return closure;
	}

	public List<LoadTimeValue> getLoadTimeValues() {
		return loadTimeValues;
	}

	public void addLoadTimeValue(final LoadTimeValue loadTimeValue) {
		loadTimeValues.add(loadTimeValue);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
