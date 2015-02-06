/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.environment.LexicalEnvironment;
import jcl.compiler.real.environment.Marker;
import jcl.compiler.real.sa.Analyzer;
import jcl.symbols.SymbolStruct;

abstract class SymbolStructAnalyzer implements Analyzer<Element, SymbolStruct<?>> {

	private static final long serialVersionUID = 4906034592734429853L;

	static LexicalEnvironment getEnclosingLambda(final LexicalEnvironment lexicalEnvironment) {

		LexicalEnvironment currentLexicalEnvironment = lexicalEnvironment;

		final Marker marker = currentLexicalEnvironment.getMarker();
		while (!Marker.LAMBDA_MARKERS.contains(marker)) {
			// TODO
			currentLexicalEnvironment = (LexicalEnvironment) currentLexicalEnvironment.getParent();
		}

		return currentLexicalEnvironment;
	}
}
