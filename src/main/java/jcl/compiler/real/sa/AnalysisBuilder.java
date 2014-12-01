/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentAccessor;
import jcl.symbols.SymbolStruct;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

public class AnalysisBuilder implements Serializable {

	private static final long serialVersionUID = 3885902321477664662L;

	private final Stack<Environment> environmentStack = new Stack<>();
	private final Stack<SymbolStruct<?>> functionNameStack = new Stack<>();

	private final Set<SymbolStruct<?>> undefinedFunctions = Collections.synchronizedSet(new HashSet<>());
	private int bindingsPosition;
	private int closureDepth;

	private final Stack<SymbolStruct<?>> blockStack = new Stack<>();
	private final Stack<Map<LispStruct, SymbolStruct<?>>> tagbodyStack = new Stack<>();

	// eval-when processing modes
	private boolean topLevelMode;

	public AnalysisBuilder() {
		bindingsPosition = 0;
		closureDepth = 0;
		topLevelMode = true;

		environmentStack.push(EnvironmentAccessor.createGlobalEnvironment());
		functionNameStack.push(null);
	}

	public Stack<Environment> getEnvironmentStack() {
		return environmentStack;
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

	public void setClosureDepth(final int closureDepth) {
		this.closureDepth = closureDepth;
	}

	public Stack<SymbolStruct<?>> getBlockStack() {
		return blockStack;
	}

	public Stack<Map<LispStruct, SymbolStruct<?>>> getTagbodyStack() {
		return tagbodyStack;
	}

	public boolean isTopLevelMode() {
		return topLevelMode;
	}

	public void setTopLevelMode(final boolean topLevelMode) {
		this.topLevelMode = topLevelMode;
	}
}
