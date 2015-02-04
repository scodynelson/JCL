/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import jcl.compiler.real.environment.DynamicEnvironment;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.GoElement;
import jcl.symbols.SymbolStruct;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

public class AnalysisBuilder implements Serializable {

	private static final long serialVersionUID = 3885902321477664662L;

	private final Stack<LexicalEnvironment> lexicalEnvironmentStack = new Stack<>();
	private final Stack<DynamicEnvironment> dynamicEnvironmentStack = new Stack<>();
	private final Stack<SymbolStruct<?>> functionNameStack = new Stack<>();

	private final Set<SymbolStruct<?>> undefinedFunctions = Collections.synchronizedSet(new HashSet<>());
	private int bindingsPosition;
	private int closureDepth;

	private final Stack<SymbolElement<?>> blockStack = new Stack<>();
	private final Stack<Set<GoElement>> tagbodyStack = new Stack<>();

	// eval-when processing modes
	private boolean topLevelMode;

	public AnalysisBuilder() {
		bindingsPosition = 0;
		closureDepth = 0;
		topLevelMode = true;

		final LexicalEnvironment globalLexicalEnvironment = new LexicalEnvironment(LexicalEnvironment.NULL, Marker.LAMBDA, 0);
		lexicalEnvironmentStack.push(globalLexicalEnvironment);

		final DynamicEnvironment globalDynamicEnvironment = new DynamicEnvironment(DynamicEnvironment.FREE);
		dynamicEnvironmentStack.push(globalDynamicEnvironment);

		functionNameStack.push(null);
	}

	public Stack<LexicalEnvironment> getLexicalEnvironmentStack() {
		return lexicalEnvironmentStack;
	}

	public Stack<DynamicEnvironment> getDynamicEnvironmentStack() {
		return dynamicEnvironmentStack;
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

	public Stack<SymbolElement<?>> getBlockStack() {
		return blockStack;
	}

	public Stack<Set<GoElement>> getTagbodyStack() {
		return tagbodyStack;
	}

	public boolean isTopLevelMode() {
		return topLevelMode;
	}

	public void setTopLevelMode(final boolean topLevelMode) {
		this.topLevelMode = topLevelMode;
	}
}
