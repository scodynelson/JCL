/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.go.GoElement;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.EnvironmentStack;
import jcl.compiler.real.environment.LambdaEnvironment;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

public class AnalysisBuilder implements Serializable {

	private static final long serialVersionUID = 3885902321477664662L;

	private final EnvironmentStack environmentStack = new EnvironmentStack();

	private final Stack<SymbolElement> functionNameStack = new Stack<>();

	private final Set<SymbolElement> undefinedFunctions = Collections.synchronizedSet(new HashSet<>());

	private final Stack<SymbolElement> blockStack = new Stack<>();

	private final Stack<Set<GoElement<?>>> tagbodyStack = new Stack<>();

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

	public Stack<SymbolElement> getFunctionNameStack() {
		return functionNameStack;
	}

	public Set<SymbolElement> getUndefinedFunctions() {
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

	public Stack<SymbolElement> getBlockStack() {
		return blockStack;
	}

	public Stack<Set<GoElement<?>>> getTagbodyStack() {
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
