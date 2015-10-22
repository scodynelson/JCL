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

import jcl.classes.StandardObjectStruct;
import jcl.compiler.real.environment.binding.Binding;
import jcl.compiler.real.environment.binding.SymbolMacroBinding;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.symbols.SymbolStruct;

public class Environment extends StandardObjectStruct {

	public static final Environment NULL = new LambdaEnvironment(null);

	private static final long serialVersionUID = 7523547599975901124L;

	private final Environment parent;

	private final List<Binding> lexicalBindings = new ArrayList<>();

	private final List<Binding> dynamicBindings = new ArrayList<>();

	private final List<SymbolMacroBinding> symbolMacroBindings = new ArrayList<>();

	private Stack<SymbolStruct<?>> functionNameStack = new Stack<>();

	private Set<SymbolStruct<?>> undefinedFunctions = Collections.synchronizedSet(new HashSet<>());

	private Stack<SymbolStruct<?>> blockStack = new Stack<>();

	private Stack<List<GoStruct<?>>> tagbodyStack = new Stack<>();

	// eval-when processing modes
	private boolean topLevelMode;

	public Environment(final Environment parent) {
		this.parent = parent;
		if (parent != null) {

			topLevelMode = parent.topLevelMode;

			functionNameStack = parent.functionNameStack;

			undefinedFunctions = parent.undefinedFunctions;

			blockStack = parent.blockStack;
			tagbodyStack = parent.tagbodyStack;
		}

		if (functionNameStack.isEmpty()) {
			functionNameStack.push(null);
		}
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

	public Stack<SymbolStruct<?>> getBlockStack() {
		return blockStack;
	}

	public Stack<List<GoStruct<?>>> getTagbodyStack() {
		return tagbodyStack;
	}

	public boolean isTopLevelMode() {
		return topLevelMode;
	}

	public void setTopLevelMode(final boolean topLevelMode) {
		this.topLevelMode = topLevelMode;
	}

	public List<Binding> getLexicalBindings() {
		return lexicalBindings;
	}

	public boolean hasLexicalBinding(final SymbolStruct<?> symbolStruct) {
		return lexicalBindings.stream()
		                      .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public Optional<Binding> getLexicalBinding(final SymbolStruct<?> symbolStruct) {
		return lexicalBindings.stream()
		                      .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		                      .findFirst();
	}

	public void addLexicalBinding(final Binding environmentBinding) {
		lexicalBindings.add(environmentBinding);
	}

	public List<Binding> getDynamicBindings() {
		return dynamicBindings;
	}

	public boolean hasDynamicBinding(final SymbolStruct<?> symbolStruct) {
		return dynamicBindings.stream()
		                      .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public Optional<Binding> getDynamicBinding(final SymbolStruct<?> symbolStruct) {
		return dynamicBindings.stream()
		                      .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		                      .findFirst();
	}

	public void addDynamicBinding(final Binding environmentBinding) {
		dynamicBindings.add(environmentBinding);
	}

	public List<SymbolMacroBinding> getSymbolMacroBindings() {
		return symbolMacroBindings;
	}

	public boolean hasSymbolMacroBinding(final SymbolStruct<?> symbolStruct) {
		return symbolMacroBindings.stream()
		                          .anyMatch(e -> e.getSymbolStruct().equals(symbolStruct));
	}

	public Optional<SymbolMacroBinding> getSymbolMacroBinding(final SymbolStruct<?> symbolStruct) {
		return symbolMacroBindings.stream()
		                          .filter(e -> e.getSymbolStruct().equals(symbolStruct))
		                          .findFirst();
	}

	public void addSymbolMacroBinding(final SymbolMacroBinding symbolMacroBinding) {
		symbolMacroBindings.add(symbolMacroBinding);
	}
}
