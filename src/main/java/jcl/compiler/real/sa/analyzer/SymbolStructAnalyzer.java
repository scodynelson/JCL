/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.LispStruct;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.Analyzer;
import jcl.symbols.SymbolStruct;

abstract class SymbolStructAnalyzer implements Analyzer<LispStruct, SymbolStruct<?>> {

	private static final long serialVersionUID = 4906034592734429853L;

	/**
	 * This method takes an environment and looks for the nearest enclosing lambda.
	 *
	 * @param lexicalEnvironment
	 * 		The environment that is enclosed by a lambda
	 *
	 * @return The lambda enclosing the given environment.
	 */
	static LexicalEnvironment getEnclosingLambda(final LexicalEnvironment lexicalEnvironment) {

		LexicalEnvironment currentLexicalEnvironment = lexicalEnvironment;

		final Marker marker = currentLexicalEnvironment.getMarker();
		while (!Marker.LAMBDA_MARKERS.contains(marker)) {
			currentLexicalEnvironment = currentLexicalEnvironment.getParent();
		}

		return currentLexicalEnvironment;
	}
}
