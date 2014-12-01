package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class ArrayStructAnalyzer implements Analyzer<LispStruct, ArrayStruct<? extends LispStruct>> {

	@SuppressWarnings("unchecked")
	@Override
	public LispStruct analyze(final SemanticAnalyzer analyzer, final ArrayStruct<? extends LispStruct> input, final AnalysisBuilder analysisBuilder) {

		// TODO: unchecked here. meh
		final List<LispStruct> inputContents = (List<LispStruct>) input.getContents();

		for (int i = 0; i < inputContents.size(); i++) {
			final LispStruct currentElement = inputContents.get(i);
			final LispStruct analyzedElement = analyzer.analyzeForm(currentElement, analysisBuilder);
			inputContents.set(i, analyzedElement);
		}

		return input;
	}
}
