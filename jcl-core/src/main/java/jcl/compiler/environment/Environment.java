/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.environment;

import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Deque;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import jcl.compiler.struct.specialoperator.go.GoStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.classes.StandardObjectStruct;
import jcl.lang.function.expander.MacroFunctionExpanderInter;
import jcl.lang.function.expander.SymbolMacroExpanderInter;
import org.apache.commons.collections4.CollectionUtils;

public class Environment extends StandardObjectStruct {

	public static final Environment NULL = new Environment(null);

	private final Environment parent;

	private static final Map<SymbolStruct, Deque<LispStruct>> lexicalSymbolBindings = new LinkedHashMap<>();

	private static final Map<SymbolStruct, Deque<LispStruct>> dynamicSymbolBindings = new LinkedHashMap<>();

	private static final Map<SymbolStruct, Deque<FunctionStruct>> lexicalFunctionBindings = new LinkedHashMap<>();

	private static final Map<SymbolStruct, Deque<MacroFunctionExpanderInter>> macroFunctionBindings = new LinkedHashMap<>();

	private static final Map<SymbolStruct, Deque<SymbolMacroExpanderInter>> symbolMacroBindings = new LinkedHashMap<>();

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

	/*
	Lexical Symbol
	 */
/*
	public Map<SymbolStruct, LispStruct> getLexicalSymbolBindings() {
		final Deque<Environment> environments = new ArrayDeque<>();
		// NULL Environment should be first, 'this' Environment should be last

		Environment current = this;
		while (NULL != current) {
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

	public LispStruct getLexicalSymbolValue(final SymbolStruct var) {
		final LispStruct value = lexicalSymbolBindings.get(var);
		if (value != null) {
			return value;
		}
		if (parent == null) {
			return var.getValue();
		}
		return parent.getLexicalSymbolValue(var);
	}

	public void setLexicalSymbolValue(final SymbolStruct var, final LispStruct val) {
		if (hasLexicalSymbolBinding(var)) {
			lexicalSymbolBindings.put(var, val);
		}
		if (parent == null) {
			lexicalSymbolBindings.put(var, val);
		} else {
			parent.setLexicalSymbolValue(var, val);
		}
	}

	public void bindLexicalValue(final SymbolStruct var, final LispStruct val) {
		lexicalSymbolBindings.put(var, val);
	}

	public void unbindLexicalValue(final SymbolStruct var) {
		lexicalSymbolBindings.remove(var);
	}
*/

	/*
	Dynamic Symbol
	 */
/*
	public Map<SymbolStruct, LispStruct> getDynamicSymbolBindings() {
		return dynamicSymbolBindings
				.entrySet()
				.stream()
				.filter(entry -> !entry.getValue().isEmpty())
				.map(entry -> Map.entry(entry.getKey(), entry.getValue().peek()))
				.collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
	}

	public boolean hasDynamicSymbolBinding(final SymbolStruct var) {
		if (dynamicSymbolBindings.containsKey(var)) {
			return !dynamicSymbolBindings.get(var).isEmpty();
		}
		return false;
	}

	public LispStruct getDynamicSymbolValue(final SymbolStruct var) {
		if (dynamicSymbolBindings.containsKey(var)) {
			final Deque<LispStruct> vals = dynamicSymbolBindings.get(var);
			if (vals.isEmpty()) {
				return var.getValue(); // TODO: is this correct??
			} else {
				return vals.peek();
			}
		} else {
			return var.getValue(); // TODO: is this correct??
		}
	}

	public void setDynamicSymbolValue(final SymbolStruct var, final LispStruct val) {
		if (dynamicSymbolBindings.containsKey(var)) {
			final Deque<LispStruct> vals = dynamicSymbolBindings.get(var);
			if (!vals.isEmpty()) {
				vals.pop();
			}
			vals.push(val);
		} else {
			final Deque<LispStruct> vals = new ArrayDeque<>();
			vals.push(val);
			dynamicSymbolBindings.put(var, vals);
		}
	}

	public void bindDynamicValue(final SymbolStruct var, final LispStruct val) {
		if (dynamicSymbolBindings.containsKey(var)) {
			dynamicSymbolBindings.get(var).push(val);
		} else {
			final Deque<LispStruct> vals = new ArrayDeque<>();
			vals.push(val);
			dynamicSymbolBindings.put(var, vals);
		}
	}

	public void unbindDynamicValue(final SymbolStruct var) {
		if (dynamicSymbolBindings.containsKey(var)) {
			dynamicSymbolBindings.get(var).pop();
		}
	}
*/
	/*
	Symbol
	 */
/*
	public LispStruct getSymbolValue(final SymbolStruct var) {
		LispStruct val;
		if (!hasLexicalSymbolBinding(var)) {
			val = getDynamicSymbolValue(var);
		} else {
			val = getLexicalSymbolValue(var);
		}
		if (val == null) {
			val = var.getValue();
		}

//		LispStruct value = getDynamicSymbolBinding(var);
//		if (value == null) {
//			value = getLexicalSymbolBinding(var);
//			if (value == null) {
//				// TODO: Symbol Macros
////				value = environment.getSymbolMacroBinding(symbol);
////				if (value == null) {
//				value = var.getValue();
////				}
//			}
//		}
		return val;
	}

	public void setSymbolValue(final SymbolStruct var, final LispStruct value) {
//		if (hasLexicalSymbolBinding(var)) {
//			bindLexicalValue(var, value);
//		} else if (hasDynamicSymbolBinding(var)) {
//			bindDynamicValue(var, value);
//		} else {
		var.setValue(value);
//		}
	}
*/
	/*
	Lexical Function
	 */
/*
	public Map<SymbolStruct, FunctionStruct> getLexicalFunctionBindings() {
		final Deque<Environment> environments = new ArrayDeque<>();
		// NULL Environment should be first, 'this' Environment should be last

		Environment current = this;
		while (NULL != current) {
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
			return var.getFunction();
		}
		return parent.getLexicalFunctionBinding(var);
	}

	public void bindFunction(final SymbolStruct var, final FunctionStruct val) {
		lexicalFunctionBindings.put(var, val);
	}

	public void unbindFunction(final SymbolStruct var) {
		lexicalFunctionBindings.remove(var);
	}

	public FunctionStruct getFunction(final SymbolStruct var) {
		FunctionStruct function = getLexicalFunctionBinding(var);
		if (function == null) {
			function = var.getFunction();
		}
		return function;
	}

	public void setFunction(final SymbolStruct var, final FunctionStruct val) {
		if (hasLexicalFunctionBinding(var)) {
			bindFunction(var, val);
		} else {
			var.setFunction(val);
		}
	}
*/
	/*
	Symbol Macro
	 */
/*
	public Map<SymbolStruct, SymbolMacroExpanderInter> getSymbolMacroBindings() {
		final Deque<Environment> environments = new ArrayDeque<>();
		// NULL Environment should be first, 'this' Environment should be last

		Environment current = this;
		while (NULL != current) {
			environments.addFirst(current);
			current = current.parent;
		}

		final Map<SymbolStruct, SymbolMacroExpanderInter> allBindings = new HashMap<>();
		for (final Environment environment : environments) {
			allBindings.putAll(environment.symbolMacroBindings);
		}
		return allBindings;
	}

	public boolean hasSymbolMacroBinding(final SymbolStruct var) {
		final boolean contains = symbolMacroBindings.containsKey(var);
		if (contains) {
			return true;
		}
		if (parent == null) {
			return false;
		}
		return parent.hasSymbolMacroBinding(var);
	}

	public SymbolMacroExpanderInter getSymbolMacroBinding(final SymbolStruct var) {
		final SymbolMacroExpanderInter value = symbolMacroBindings.get(var);
		if (value != null) {
			return value;
		}
		if (parent == null) {
			return var.getSymbolMacroExpander();
		}
		return parent.getSymbolMacroBinding(var);
	}

	public void bindSymbolMacro(final SymbolStruct var, final SymbolMacroExpanderInter val) {
		symbolMacroBindings.put(var, val);
	}

	public void unbindSymbolMacro(final SymbolStruct var) {
		symbolMacroBindings.remove(var);
	}
*/

	public Map<SymbolStruct, Deque<LispStruct>> getLexicalSymbolBindings() {
		return lexicalSymbolBindings;
	}

	public Map<SymbolStruct, Deque<LispStruct>> getDynamicSymbolBindings() {
		return dynamicSymbolBindings;
	}

	public Map<SymbolStruct, Deque<FunctionStruct>> getLexicalFunctionBindings() {
		return lexicalFunctionBindings;
	}

	public Map<SymbolStruct, Deque<MacroFunctionExpanderInter>> getMacroFunctionBindings() {
		return macroFunctionBindings;
	}

	public Map<SymbolStruct, Deque<SymbolMacroExpanderInter>> getSymbolMacroBindings() {
		return symbolMacroBindings;
	}

	public LispStruct getSymbolValue(final SymbolStruct var) {
		final LispStruct value;
		if (CollectionUtils.isEmpty(lexicalSymbolBindings.get(var))) {
			value = getDynamicSymbolValue(var);
		} else {
			value = getLexicalSymbolValue(var);
		}

		if (value == null) {
			return var.symbolValue();
		}

		return value;
	}

	public LispStruct getLexicalSymbolValue(final SymbolStruct var) {
		if (CollectionUtils.isEmpty(lexicalSymbolBindings.get(var))) {
			return var.symbolValue();
		}

		final LispStruct value = lexicalSymbolBindings.get(var).peek();
		if (value == null) {
			return var.symbolValue();
		}

		return value;
	}

	public LispStruct getDynamicSymbolValue(final SymbolStruct var) {
		if (CollectionUtils.isEmpty(dynamicSymbolBindings.get(var))) {
			return var.symbolValue();
		}

		final LispStruct value = dynamicSymbolBindings.get(var).peek();
		if (value == null) {
			return var.symbolValue();
		}

		return value;
	}

	public void setSymbolValue(final SymbolStruct var, final LispStruct value) {
		// TODO: handle constants
		if (CollectionUtils.isEmpty(lexicalSymbolBindings.get(var))) {
			if (CollectionUtils.isEmpty(dynamicSymbolBindings.get(var))) {
				var.setSymbolValue(value);
			} else {
				setDynamicSymbolValue(var, value);
			}
		} else {
			setLexicalSymbolValue(var, value);
		}
	}

	public void setLexicalSymbolValue(final SymbolStruct var, final LispStruct value) {
		// TODO: handle constants
		if (CollectionUtils.isEmpty(lexicalSymbolBindings.get(var))) {
			lexicalSymbolBindings.get(var).push(value);
		} else {
			lexicalSymbolBindings.get(var).pop();
			lexicalSymbolBindings.get(var).push(value);
		}
	}

	public void setDynamicSymbolValue(final SymbolStruct var, final LispStruct value) {
		// TODO: handle constants
		if (CollectionUtils.isEmpty(dynamicSymbolBindings.get(var))) {
			dynamicSymbolBindings.get(var).push(value);
		} else {
			dynamicSymbolBindings.get(var).pop();
			dynamicSymbolBindings.get(var).push(value);
		}
	}

	public void bindLexicalValue(final SymbolStruct var, final LispStruct value) {
		// TODO: handle constants
		if (value == null) {
			if (!lexicalSymbolBindings.containsKey(var)) {
				lexicalSymbolBindings.put(var, new ArrayDeque<>());
			}
		} else {
			if (CollectionUtils.isEmpty(lexicalSymbolBindings.get(var))) {
				final Deque<LispStruct> values = new ArrayDeque<>();
				values.push(value);
				lexicalSymbolBindings.put(var, values);
			} else {
				lexicalSymbolBindings.get(var).push(value);
			}
		}
	}

	public void unbindLexicalValue(final SymbolStruct var) {
		// TODO: handle constants
		if (!CollectionUtils.isEmpty(lexicalSymbolBindings.get(var))) {
			lexicalSymbolBindings.get(var).pop();
		}
	}

	public void bindDynamicValue(final SymbolStruct var, final LispStruct value) {
		// TODO: handle constants
		if (value == null) {
			if (!dynamicSymbolBindings.containsKey(var)) {
				dynamicSymbolBindings.put(var, new ArrayDeque<>());
			}
		} else {
			if (CollectionUtils.isEmpty(dynamicSymbolBindings.get(var))) {
				final Deque<LispStruct> values = new ArrayDeque<>();
				values.push(value);
				dynamicSymbolBindings.put(var, values);
			} else {
				dynamicSymbolBindings.get(var).push(value);
			}
		}
	}

	public void unbindDynamicValue(final SymbolStruct var) {
		// TODO: handle constants
		if (!CollectionUtils.isEmpty(dynamicSymbolBindings.get(var))) {
			dynamicSymbolBindings.get(var).pop();
		}
	}

	public boolean hasFunction(final SymbolStruct var) {
		return !CollectionUtils.isEmpty(lexicalFunctionBindings.get(var))
				|| var.fBoundP().toJavaPBoolean();
	}

	public FunctionStruct getFunction(final SymbolStruct var) {
		if (CollectionUtils.isEmpty(lexicalFunctionBindings.get(var))) {
			return var.symbolFunction();
		}
		return lexicalFunctionBindings.get(var).peek();
	}

	public void setFunction(final SymbolStruct var, final FunctionStruct function) {
		if (CollectionUtils.isEmpty(lexicalFunctionBindings.get(var))) {
			lexicalFunctionBindings.get(var).push(function);
		} else {
			lexicalFunctionBindings.get(var).pop();
			lexicalFunctionBindings.get(var).push(function);
		}
	}

	public void bindFunction(final SymbolStruct var, final FunctionStruct function) {
		if (function == null) {
			if (!lexicalFunctionBindings.containsKey(var)) {
				lexicalFunctionBindings.put(var, new ArrayDeque<>());
			}
		} else {
			if (CollectionUtils.isEmpty(lexicalFunctionBindings.get(var))) {
				final Deque<FunctionStruct> values = new ArrayDeque<>();
				values.push(function);
				lexicalFunctionBindings.put(var, values);
			} else {
				lexicalFunctionBindings.get(var).push(function);
			}
		}
	}

	public void unbindFunction(final SymbolStruct var) {
		if (!CollectionUtils.isEmpty(lexicalFunctionBindings.get(var))) {
			lexicalFunctionBindings.get(var).pop();
		}
	}

	public boolean hasMacroFunctionExpander(final SymbolStruct var) {
		return !CollectionUtils.isEmpty(macroFunctionBindings.get(var))
				|| (SymbolStruct.getMacroFunctionDefinition(var) != null);
	}

	public MacroFunctionExpanderInter getMacroFunctionExpander(final SymbolStruct var) {
		if (CollectionUtils.isEmpty(macroFunctionBindings.get(var))) {
			return SymbolStruct.getMacroFunctionDefinition(var);
		}
		return macroFunctionBindings.get(var).peek();
	}

	public void setMacroFunctionExpander(final SymbolStruct var, final MacroFunctionExpanderInter macroFunction) {
		if (CollectionUtils.isEmpty(macroFunctionBindings.get(var))) {
			macroFunctionBindings.get(var).push(macroFunction);
		} else {
			macroFunctionBindings.get(var).pop();
			macroFunctionBindings.get(var).push(macroFunction);
		}
	}

	public void bindMacroFunctionExpander(final SymbolStruct var, final MacroFunctionExpanderInter macroFunction) {
		if (macroFunction == null) {
			if (!macroFunctionBindings.containsKey(var)) {
				macroFunctionBindings.put(var, new ArrayDeque<>());
			}
		} else {
			if (CollectionUtils.isEmpty(macroFunctionBindings.get(var))) {
				final Deque<MacroFunctionExpanderInter> values = new ArrayDeque<>();
				values.push(macroFunction);
				macroFunctionBindings.put(var, values);
			} else {
				macroFunctionBindings.get(var).push(macroFunction);
			}
		}
	}

	public void unbindMacroFunctionExpander(final SymbolStruct var) {
		if (!CollectionUtils.isEmpty(macroFunctionBindings.get(var))) {
			macroFunctionBindings.get(var).pop();
		}
	}

	public boolean hasSymbolMacroExpander(final SymbolStruct var) {
		return !CollectionUtils.isEmpty(symbolMacroBindings.get(var))
				|| (SymbolStruct.getSymbolMacroDefinition(var) != null);
	}

	public SymbolMacroExpanderInter getSymbolMacroExpander(final SymbolStruct var) {
		if (CollectionUtils.isEmpty(symbolMacroBindings.get(var))) {
			return null;
		}
		return symbolMacroBindings.get(var).peek();
	}

	public void setSymbolMacroExpander(final SymbolStruct var, final SymbolMacroExpanderInter symbolMacro) {
		if (CollectionUtils.isEmpty(symbolMacroBindings.get(var))) {
			symbolMacroBindings.get(var).push(symbolMacro);
		} else {
			symbolMacroBindings.get(var).pop();
			symbolMacroBindings.get(var).push(symbolMacro);
		}
	}

	public void bindSymbolMacroExpander(final SymbolStruct var, final SymbolMacroExpanderInter symbolMacro) {
		if (symbolMacro == null) {
			if (!symbolMacroBindings.containsKey(var)) {
				symbolMacroBindings.put(var, new ArrayDeque<>());
			}
		} else {
			if (CollectionUtils.isEmpty(symbolMacroBindings.get(var))) {
				final Deque<SymbolMacroExpanderInter> values = new ArrayDeque<>();
				values.push(symbolMacro);
				symbolMacroBindings.put(var, values);
			} else {
				symbolMacroBindings.get(var).push(symbolMacro);
			}
		}
	}

	public void unbindSymbolMacroExpander(final SymbolStruct var) {
		if (!CollectionUtils.isEmpty(symbolMacroBindings.get(var))) {
			symbolMacroBindings.get(var).pop();
		}
	}
}
