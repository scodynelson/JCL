/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa;

import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.element.SymbolElement;
import jcl.symbols.SymbolStruct;

abstract class SymbolStructAnalyzer {

	public abstract SymbolElement<?> analyzeSymbol(final SymbolStruct<?> input, final AnalysisBuilder analysisBuilder);

	/**
	 * This method takes an environment and looks for the nearest enclosing lambda.
	 *
	 * @param lexicalEnvironment
	 * 		The environment that is enclosed by a lambda
	 *
	 * @return The lambda enclosing the given environment.
	 */
	protected static LexicalEnvironment getEnclosingLambda(final LexicalEnvironment lexicalEnvironment) {

		LexicalEnvironment currentLexicalEnvironment = lexicalEnvironment;

		final Marker marker = currentLexicalEnvironment.getMarker();
		while (!Marker.LAMBDA_MARKERS.contains(marker)) {
			currentLexicalEnvironment = currentLexicalEnvironment.getParent();
		}

		return currentLexicalEnvironment;
	}
}
