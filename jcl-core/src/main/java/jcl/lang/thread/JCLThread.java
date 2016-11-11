package jcl.lang.thread;

import java.util.Map;
import java.util.Stack;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ErrorException;

public final class JCLThread {

	private static final Map<Thread, Map<SymbolStruct, Stack<LispStruct>>> SPECIAL_SYMBOL_VALUES
			= new ConcurrentHashMap<>();

	private JCLThread() {
	}

	public static LispStruct getDynamicValue(final SymbolStruct symbol) {
		final Thread currentThread = Thread.currentThread();

		final Map<SymbolStruct, Stack<LispStruct>> dynamicBindings = SPECIAL_SYMBOL_VALUES.get(currentThread);
		if (dynamicBindings == null) {
			throw new ErrorException("Unbound variable: " + symbol);
		}

		final Stack<LispStruct> dynamicValues = dynamicBindings.get(symbol);
		if (dynamicValues == null) {
			throw new ErrorException("Unbound variable: " + symbol);
		}

		if (dynamicValues.isEmpty()) {
			throw new ErrorException("Unbound variable: " + symbol);
		}

		final LispStruct value = dynamicValues.peek();
		if (value == null) {
			throw new ErrorException("Unbound variable: " + symbol);
		}

		return value;
	}

	public static void setDynamicValue(final SymbolStruct symbol, final LispStruct value) {
		final Thread currentThread = Thread.currentThread();

		Map<SymbolStruct, Stack<LispStruct>> dynamicBindings = SPECIAL_SYMBOL_VALUES.get(currentThread);
		if (dynamicBindings == null) {
			dynamicBindings = new ConcurrentHashMap<>();
			SPECIAL_SYMBOL_VALUES.put(currentThread, dynamicBindings);
		}

		Stack<LispStruct> dynamicValues = dynamicBindings.get(symbol);
		if (dynamicValues == null) {
			dynamicValues = new Stack<>();
			dynamicBindings.put(symbol, dynamicValues);
		}

		if (dynamicValues.isEmpty()) {
			dynamicValues.push(value);
		} else {
			dynamicValues.pop();
			dynamicValues.push(value);
		}
	}

	public static void bindDynamicValue(final SymbolStruct symbol, final LispStruct value) {
		final Thread currentThread = Thread.currentThread();

		Map<SymbolStruct, Stack<LispStruct>> dynamicBindings = SPECIAL_SYMBOL_VALUES.get(currentThread);
		if (dynamicBindings == null) {
			dynamicBindings = new ConcurrentHashMap<>();
			SPECIAL_SYMBOL_VALUES.put(currentThread, dynamicBindings);
		}

		Stack<LispStruct> dynamicValues = dynamicBindings.get(symbol);
		if (dynamicValues == null) {
			dynamicValues = new Stack<>();
			dynamicBindings.put(symbol, dynamicValues);
		}

		dynamicValues.push(value);
	}

	public static void unbindDynamicValue(final SymbolStruct symbol) {
		final Thread currentThread = Thread.currentThread();

		final Map<SymbolStruct, Stack<LispStruct>> dynamicBindings = SPECIAL_SYMBOL_VALUES.get(currentThread);
		if (dynamicBindings == null) {
			throw new ErrorException("Unbound variable: " + symbol);
		}

		final Stack<LispStruct> dynamicValues = dynamicBindings.get(symbol);
		if (dynamicValues == null) {
			throw new ErrorException("Unbound variable: " + symbol);
		}

		if (dynamicValues.isEmpty()) {
			throw new ErrorException("Unbound variable: " + symbol);
		} else {
			dynamicValues.pop();
		}
	}

}
