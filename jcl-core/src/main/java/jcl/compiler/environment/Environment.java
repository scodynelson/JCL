/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import jcl.compiler.environment.binding.Binding;
import jcl.compiler.environment.binding.SymbolMacroBinding;
import jcl.compiler.struct.specialoperator.go.GoStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.classes.StandardObjectStruct;

public class Environment extends StandardObjectStruct {

	public static final Environment NULL = new Environment(null);

	private final Environment parent;

	private final Map<SymbolStruct, LispStruct> lexicalSymbolBindings = new LinkedHashMap<>();

	private final Map<SymbolStruct, FunctionStruct> lexicalFunctionBindings = new LinkedHashMap<>();

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

	public Map<SymbolStruct, LispStruct> getLexicalSymbolBindings() {
		final Deque<Environment> environments = new ArrayDeque<>();
		// NULL Environment should be first, 'this' Environment should be last

		Environment current = this;
		while(NULL != current) {
			environments.addFirst(current);
			current = current.parent;
		}

		final Map<SymbolStruct, LispStruct> allBindings = new HashMap<>();
		for (final Environment environment : environments) {
			allBindings.putAll(environment.lexicalSymbolBindings);
		}
		return allBindings;
	}

	public boolean hasLexicalSymbolBinding(final SymbolStruct var) {
		final boolean contains = lexicalSymbolBindings.containsKey(var);
		if (contains) {
			return true;
		}
		if (parent == null) {
			return false;
		}
		return parent.hasLexicalSymbolBinding(var);
	}

	public LispStruct getLexicalSymbolBinding(final SymbolStruct var) {
		final LispStruct value = lexicalSymbolBindings.get(var);
		if (value != null) {
			return value;
		}
		if (parent == null) {
			return null;
		}
		return parent.getLexicalSymbolBinding(var);
	}

	public void bindLexicalValue(final SymbolStruct var, final LispStruct val) {
		lexicalSymbolBindings.put(var, val);
	}

	public void unbindLexicalValue(final SymbolStruct var) {
		lexicalSymbolBindings.remove(var);
	}

	public LispStruct getSymbolValue(final SymbolStruct var) {
		LispStruct value = var.getDynamicValue();
		if (value == null) {
			value = getLexicalSymbolBinding(var);
			if (value == null) {
				// TODO: Symbol Macros
//				value = environment.getSymbolMacroBinding(symbol);
//				if (value == null) {
				value = var.getValue();
//				}
			}
		}
		return value;
	}

	public void setSymbolValue(final SymbolStruct var, final LispStruct value) {
		if (hasLexicalSymbolBinding(var)) {
			bindLexicalValue(var, value);
		} else {
			var.setValue(value);
		}
	}

	public Map<SymbolStruct, FunctionStruct> getLexicalFunctionBindings() {
		final Deque<Environment> environments = new ArrayDeque<>();
		// NULL Environment should be first, 'this' Environment should be last

		Environment current = this;
		while(NULL != current) {
			environments.addFirst(current);
			current = current.parent;
		}

		final Map<SymbolStruct, FunctionStruct> allBindings = new HashMap<>();
		for (final Environment environment : environments) {
			allBindings.putAll(environment.lexicalFunctionBindings);
		}
		return allBindings;
	}

	public boolean hasLexicalFunctionBinding(final SymbolStruct var) {
		final boolean contains = lexicalFunctionBindings.containsKey(var);
		if (contains) {
			return true;
		}
		if (parent == null) {
			return false;
		}
		return parent.hasLexicalFunctionBinding(var);
	}

	public FunctionStruct getLexicalFunctionBinding(final SymbolStruct var) {
		final FunctionStruct value = lexicalFunctionBindings.get(var);
		if (value != null) {
			return value;
		}
		if (parent == null) {
			return null;
		}
		return parent.getLexicalFunctionBinding(var);
	}

	public FunctionStruct getFunction(final SymbolStruct var) {
		FunctionStruct function = getLexicalFunctionBinding(var);
		if (function == null) {
			function = var.getFunction();
		}
		return function;
	}

	public void bindFunction(final SymbolStruct var, final FunctionStruct val) {
		lexicalFunctionBindings.put(var, val);
	}

	public void unbindFunction(final SymbolStruct var) {
		lexicalFunctionBindings.remove(var);
	}

	public boolean hasLexicalBinding(final SymbolStruct var) {
		return lexicalBindings.stream()
		                      .anyMatch(e -> e.getVar().eq(var));
	}

	public void addLexicalBinding(final Binding environmentBinding) {
		lexicalBindings.add(environmentBinding);
	}

	public boolean hasDynamicBinding(final SymbolStruct var) {
		return dynamicBindings.stream()
		                      .anyMatch(e -> e.getVar().eq(var));
	}

	public void addDynamicBinding(final Binding environmentBinding) {
		dynamicBindings.add(environmentBinding);
	}

	public boolean hasSymbolMacroBinding(final SymbolStruct var) {
		return symbolMacroBindings.stream()
		                          .anyMatch(e -> e.getVar().eq(var));
	}

	public SymbolMacroBinding getSymbolMacroBinding(final SymbolStruct var) {
		return symbolMacroBindings.stream()
		                          .filter(e -> e.getVar().eq(var))
		                          .findAny()
		                          .orElse(null);
	}

	public void addSymbolMacroBinding(final SymbolMacroBinding symbolMacroBinding) {
		symbolMacroBindings.add(symbolMacroBinding);
	}
}
