/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.environment;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.EnvironmentBinding;
import jcl.compiler.real.environment.binding.EnvironmentParameterBinding;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class Environment implements LispStruct {

	public static final Environment NULL = new LambdaEnvironment(null);

	private static final long serialVersionUID = 7523547599975901124L;

	private final Environment parent;

	private final List<EnvironmentParameterBinding> lexicalBindings = new ArrayList<>();

	private final List<EnvironmentBinding<?>> dynamicBindings = new ArrayList<>();

	private final SymbolTable symbolTable = new SymbolTable();

	private final Closure closure;

	private int bindingsPosition;

	private int closureDepth = -1;

	private Stack<SymbolStruct<?>> functionNameStack = new Stack<>();

	private Set<SymbolStruct<?>> undefinedFunctions = Collections.synchronizedSet(new HashSet<>());

	private Stack<SymbolStruct<?>> blockStack = new Stack<>();

	private Stack<Set<GoStruct<?>>> tagbodyStack = new Stack<>();

	// eval-when processing modes
	private boolean topLevelMode;

	protected Environment(final Environment parent) {
		this.parent = parent;
		closureDepth = parent.closureDepth + 1;
		closure = new Closure(closureDepth);

		bindingsPosition = parent.bindingsPosition;
		topLevelMode = parent.topLevelMode;

		functionNameStack = parent.functionNameStack;
		if (functionNameStack.isEmpty()) {
			functionNameStack.push(null);
		}

		undefinedFunctions = parent.undefinedFunctions;
		blockStack = parent.blockStack;
		tagbodyStack = parent.tagbodyStack;
	}

	public Environment getParent() {
		return parent;
	}

	public Stack<SymbolStruct<?>> getFunctionNameStack() {
		return functionNameStack;
	}

	public Set<SymbolStruct<?>> getUndefinedFunctions() {
		return undefinedFunctions;
	}

	public int getBindingsPosition() {
		return bindingsPosition;
	}

	public void setBindingsPosition(final int bindingsPosition) {
		this.bindingsPosition = bindingsPosition;
	}

	public int getClosureDepth() {
		return closureDepth;
	}

	public Stack<SymbolStruct<?>> getBlockStack() {
		return blockStack;
	}

	public Stack<Set<GoStruct<?>>> getTagbodyStack() {
		return tagbodyStack;
	}

	public boolean isTopLevelMode() {
		return topLevelMode;
	}

	public void setTopLevelMode(final boolean topLevelMode) {
		this.topLevelMode = topLevelMode;
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
		return new ReflectionToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).setExcludeFieldNames("parent").toString();
	}
}
