/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import jcl.classes.StandardObjectStruct;
import jcl.compiler.environment.binding.Binding;
import jcl.compiler.environment.binding.SymbolMacroBinding;
import jcl.compiler.struct.specialoperator.go.GoStruct;
import jcl.symbols.SymbolStruct;

public class Environment extends StandardObjectStruct {

	public static final Environment NULL = new Environment(null);

	private final Environment parent;

	private final List<Binding> lexicalBindings = new ArrayList<>();

	private final List<Binding> dynamicBindings = new ArrayList<>();

	private final List<SymbolMacroBinding> symbolMacroBindings = new ArrayList<>();

	private Stack<SymbolStruct> functionNameStack = new Stack<>();

	private Set<SymbolStruct> undefinedFunctions = Collections.synchronizedSet(new HashSet<>());

	private Stack<SymbolStruct> blockStack = new Stack<>();

	private Stack<List<GoStruct<?>>> tagbodyStack = new Stack<>();

	public Environment(final Environment parent) {
		this.parent = parent;
		if (parent != null) {
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

	public Stack<SymbolStruct> getFunctionNameStack() {
		return functionNameStack;
	}

	public Set<SymbolStruct> getUndefinedFunctions() {
		return undefinedFunctions;
	}

	public Stack<SymbolStruct> getBlockStack() {
		return blockStack;
	}

	public Stack<List<GoStruct<?>>> getTagbodyStack() {
		return tagbodyStack;
	}

	public boolean hasLexicalBinding(final SymbolStruct var) {
		return lexicalBindings.stream()
		                      .anyMatch(e -> e.getVar().equals(var));
	}

	public void addLexicalBinding(final Binding environmentBinding) {
		lexicalBindings.add(environmentBinding);
	}

	public boolean hasDynamicBinding(final SymbolStruct var) {
		return dynamicBindings.stream()
		                      .anyMatch(e -> e.getVar().equals(var));
	}

	public void addDynamicBinding(final Binding environmentBinding) {
		dynamicBindings.add(environmentBinding);
	}

	public boolean hasSymbolMacroBinding(final SymbolStruct var) {
		return symbolMacroBindings.stream()
		                          .anyMatch(e -> e.getVar().equals(var));
	}

	public void addSymbolMacroBinding(final SymbolMacroBinding symbolMacroBinding) {
		symbolMacroBindings.add(symbolMacroBinding);
	}
}
