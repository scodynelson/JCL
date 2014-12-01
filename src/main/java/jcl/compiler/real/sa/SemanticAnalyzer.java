/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.symbols.SymbolStruct;

import java.util.Map;
import java.util.Set;
import java.util.Stack;

public interface SemanticAnalyzer {

	LispStruct analyze(final LispStruct form);

	LispStruct analyzeForm(final LispStruct form);

	Stack<Environment> getEnvironmentStack();

	Set<SymbolStruct<?>> getUndefinedFunctions();

	Stack<SymbolStruct<?>> getFunctionNameStack();

	int getBindingsPosition();

	void setBindingsPosition(final int bindingsPosition);

	int getClosureDepth();

	void setClosureDepth(final int closureDepth);

	Stack<SymbolStruct<?>> getBlockStack();

	Stack<Map<LispStruct, SymbolStruct<?>>> getTagbodyStack();

	boolean isTopLevelMode();

	void setTopLevelMode(final boolean topLevelMode);
}
