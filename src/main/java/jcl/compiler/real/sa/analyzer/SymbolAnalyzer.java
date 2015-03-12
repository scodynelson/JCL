/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.symbols.SymbolStruct;

public interface SymbolAnalyzer extends Analyzer<LispStruct, SymbolStruct<?>> {

	SymbolStruct<?> analyzeLexical(final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder);

	SymbolStruct<?> analyzeDynamic(final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder);
}