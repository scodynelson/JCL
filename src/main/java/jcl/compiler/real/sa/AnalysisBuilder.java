/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.LambdaEnvironment;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class AnalysisBuilder implements Serializable {

	private static final long serialVersionUID = 3885902321477664662L;

	private final EnvironmentStack environmentStack = new EnvironmentStack();

	private final Stack<SymbolStruct<?>> functionNameStack = new Stack<>();

	private final Set<SymbolStruct<?>> undefinedFunctions = Collections.synchronizedSet(new HashSet<>());

	private final Stack<SymbolStruct<?>> blockStack = new Stack<>();

	private final Stack<Set<GoStruct<?>>> tagbodyStack = new Stack<>();

	private int bindingsPosition;

	private int closureDepth;

	// eval-when processing modes
	private boolean topLevelMode;

	public AnalysisBuilder() {
		bindingsPosition = 0;
		closureDepth = 0;
		topLevelMode = true;

		final Environment globalEnvironment = new LambdaEnvironment(Environment.NULL, 0);
		environmentStack.push(globalEnvironment);

		functionNameStack.push(null);
	}

	public EnvironmentStack getEnvironmentStack() {
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

	public Stack<Set<GoStruct<?>>> getTagbodyStack() {
		return tagbodyStack;
	}

	public boolean isTopLevelMode() {
		return topLevelMode;
	}

	public void setTopLevelMode(final boolean topLevelMode) {
		this.topLevelMode = topLevelMode;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
