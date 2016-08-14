package jcl.compiler.symbol;

import java.util.Map;
import java.util.Stack;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;
import jcl.lang.internal.StructureClassStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.function.expander.CompilerMacroFunctionExpanderInter;
import jcl.lang.function.expander.MacroFunctionExpanderInter;
import jcl.lang.function.expander.SymbolMacroExpanderInter;
import jcl.lang.statics.PackageVariables;

public final class SymbolState {

	public static final Map<SymbolStruct, SymbolState> SYMBOL_STATE = new ConcurrentHashMap<>();

	private Stack<LispStruct> lexicalValueStack = new Stack<>();

	private Stack<LispStruct> dynamicValueStack = new Stack<>();

	private Stack<FunctionStruct> functionStack = new Stack<>();

	private MacroFunctionExpanderInter macroFunctionExpander;

	private CompilerMacroFunctionExpanderInter compilerMacroFunctionExpander;

	private Stack<SymbolMacroExpanderInter> symbolMacroExpanderStack = new Stack<>();

	private StructureClassStruct structureClass;

	public Stack<LispStruct> getLexicalValueStack() {
		return lexicalValueStack;
	}

	public void setLexicalValueStack(final Stack<LispStruct> lexicalValueStack) {
		this.lexicalValueStack = lexicalValueStack;
	}

	public Stack<LispStruct> getDynamicValueStack() {
		return dynamicValueStack;
	}

	public void setDynamicValueStack(final Stack<LispStruct> dynamicValueStack) {
		this.dynamicValueStack = dynamicValueStack;
	}

	public Stack<FunctionStruct> getFunctionStack() {
		return functionStack;
	}

	public void setFunctionStack(final Stack<FunctionStruct> functionStack) {
		this.functionStack = functionStack;
	}

	public MacroFunctionExpanderInter getMacroFunctionExpander() {
		return macroFunctionExpander;
	}

	public void setMacroFunctionExpander(final MacroFunctionExpanderInter macroFunctionExpander) {
		this.macroFunctionExpander = macroFunctionExpander;
	}

	public CompilerMacroFunctionExpanderInter getCompilerMacroFunctionExpander() {
		return compilerMacroFunctionExpander;
	}

	public void setCompilerMacroFunctionExpander(final CompilerMacroFunctionExpanderInter compilerMacroFunctionExpander) {
		this.compilerMacroFunctionExpander = compilerMacroFunctionExpander;
	}

	public Stack<SymbolMacroExpanderInter> getSymbolMacroExpanderStack() {
		return symbolMacroExpanderStack;
	}

	public void setSymbolMacroExpanderStack(final Stack<SymbolMacroExpanderInter> symbolMacroExpanderStack) {
		this.symbolMacroExpanderStack = symbolMacroExpanderStack;
	}

	public StructureClassStruct getStructureClass() {
		return structureClass;
	}

	public void setStructureClass(final StructureClassStruct structureClass) {
		this.structureClass = structureClass;
	}

	/*
		OLD-SYMBOL_STRUCT CODE
	 */

	public static boolean hasValue(final SymbolStruct symbol) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		return !symbolState.lexicalValueStack.isEmpty() || !symbolState.dynamicValueStack.isEmpty();
	}

	public static LispStruct getValue(final SymbolStruct symbol) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		final LispStruct value;
		if (symbolState.lexicalValueStack.isEmpty()) {
			value = getDynamicValue(symbol);
		} else {
			value = getLexicalValue(symbol);
		}

		if (value == null) {
			return handleUnboundValue(symbol);
		}

		return value;
	}

	public static LispStruct getLexicalValue(final SymbolStruct symbol) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		if (symbolState.lexicalValueStack.isEmpty()) {
			return handleUnboundValue(symbol);
		}

		final LispStruct value = symbolState.lexicalValueStack.peek();
		if (value == null) {
			return handleUnboundValue(symbol);
		}

		return value;
	}

	public static LispStruct getDynamicValue(final SymbolStruct symbol) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		if (symbolState.dynamicValueStack.isEmpty()) {
			handleUnboundValue(symbol);
		}

		final LispStruct value = symbolState.dynamicValueStack.peek();
		if (value == null) {
			return handleUnboundValue(symbol);
		}

		return value;
	}

	private static LispStruct handleUnboundValue(final SymbolStruct symbol) {
		final String name = symbol.getName();
		String variableName = name;
		final PackageStruct currentPackage = PackageVariables.PACKAGE.getVariableValue();

		final PackageStruct symbolPackage = symbol.getSymbolPackage();
		if (!currentPackage.equals(symbolPackage)) {
			if (symbolPackage == null) {
				variableName = "#:" + name;
			} else {
				final String packageName = symbolPackage.getName();
				if (currentPackage.getExternalSymbols().containsKey(name)) {
					variableName = packageName + ':' + name;
				} else {
					variableName = packageName + "::" + name;
				}
			}
		}

		throw new ErrorException("Unbound variable: " + variableName);
	}

	public static void setValue(final SymbolStruct symbol, final LispStruct value) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		if (symbolState.lexicalValueStack.isEmpty()) {
			if (symbolState.dynamicValueStack.isEmpty()) {
				symbolState.dynamicValueStack.push(value);
			} else {
				symbolState.dynamicValueStack.pop();
				symbolState.dynamicValueStack.push(value);
			}
		} else {
			symbolState.lexicalValueStack.pop();
			symbolState.lexicalValueStack.push(value);
		}
	}

	public static void setLexicalValue(final SymbolStruct symbol, final LispStruct value) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		if (symbolState.lexicalValueStack.isEmpty()) {
			symbolState.lexicalValueStack.push(value);
		} else {
			symbolState.lexicalValueStack.pop();
			symbolState.lexicalValueStack.push(value);
		}
	}

	public static void setDynamicValue(final SymbolStruct symbol, final LispStruct value) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		if (symbolState.dynamicValueStack.isEmpty()) {
			symbolState.dynamicValueStack.push(value);
		} else {
			symbolState.dynamicValueStack.pop();
			symbolState.dynamicValueStack.push(value);
		}
	}

	public static void bindLexicalValue(final SymbolStruct symbol, final LispStruct value) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		symbolState.lexicalValueStack.push(value);
	}

	public static void unbindLexicalValue(final SymbolStruct symbol) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		symbolState.lexicalValueStack.pop();
	}

	public static void bindDynamicValue(final SymbolStruct symbol, final LispStruct value) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		symbolState.dynamicValueStack.push(value);
	}

	public static void unbindDynamicValue(final SymbolStruct symbol) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		symbolState.dynamicValueStack.pop();
	}

	public static boolean hasFunction(final SymbolStruct symbol) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		return !symbolState.functionStack.isEmpty();
	}

	public static FunctionStruct getFunction(final SymbolStruct symbol) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		if (symbolState.functionStack.isEmpty()) {
			return handleUnboundFunction(symbol);
		}
		return symbolState.functionStack.peek();
	}

	private static FunctionStruct handleUnboundFunction(final SymbolStruct symbol) {
		final String name = symbol.getName();
		String variableName = name;
		final PackageStruct currentPackage = PackageVariables.PACKAGE.getVariableValue();

		final PackageStruct symbolPackage = symbol.getSymbolPackage();
		if (!currentPackage.equals(symbolPackage)) {
			if (symbolPackage == null) {
				variableName = "#:" + name;
			} else {
				final String packageName = symbolPackage.getName();
				if (currentPackage.getExternalSymbols().containsKey(name)) {
					variableName = packageName + ':' + name;
				} else {
					variableName = packageName + "::" + name;
				}
			}
		}

		throw new ErrorException("Undefined function: " + variableName);
	}

	public static void setFunction(final SymbolStruct symbol, final FunctionStruct function) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		if (symbolState.functionStack.isEmpty()) {
			symbolState.functionStack.push(function);
		} else {
			symbolState.functionStack.pop();
			symbolState.functionStack.push(function);
		}
	}

	public static void bindFunction(final SymbolStruct symbol, final FunctionStruct function) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		symbolState.functionStack.push(function);
	}

	public static FunctionStruct unbindFunction(final SymbolStruct symbol) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		return symbolState.functionStack.pop();
	}

	public static SymbolMacroExpanderInter getSymbolMacroExpander(final SymbolStruct symbol) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		if (symbolState.symbolMacroExpanderStack.isEmpty()) {
			return null;
		}
		return symbolState.symbolMacroExpanderStack.peek();
	}

	public static void setSymbolMacroExpander(final SymbolStruct symbol, final SymbolMacroExpanderInter symbolMacroExpander) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		symbolState.symbolMacroExpanderStack.pop();
		symbolState.symbolMacroExpanderStack.push(symbolMacroExpander);
	}

	public static void bindSymbolMacroExpander(final SymbolStruct symbol, final SymbolMacroExpanderInter symbolMacroExpander) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		symbolState.symbolMacroExpanderStack.push(symbolMacroExpander);
	}

	public static void unbindSymbolMacroExpander(final SymbolStruct symbol) {
		final SymbolState symbolState = SYMBOL_STATE.get(symbol);

		symbolState.symbolMacroExpanderStack.pop();
	}
}
