/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.analyzer;

import jcl.characters.CharacterStruct;
import jcl.compiler.real.element.CharacterElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import org.springframework.stereotype.Component;

@Component
public class CharacterAnalyzer implements Analyzer<CharacterElement, CharacterStruct> {

	private static final long serialVersionUID = 2303128205313311050L;

	@Override
	public CharacterElement analyze(final SemanticAnalyzer analyzer, final CharacterStruct input, final AnalysisBuilder analysisBuilder) {
		return new CharacterElement(input);
	}
}
