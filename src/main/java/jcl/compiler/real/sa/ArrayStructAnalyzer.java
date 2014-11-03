package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.structs.arrays.ArrayStruct;

import java.util.List;

public class ArrayStructAnalyzer implements Analyzer<LispStruct, ArrayStruct<? extends LispStruct>> {

	public static final ArrayStructAnalyzer INSTANCE = new ArrayStructAnalyzer();

	@SuppressWarnings("unchecked")
	@Override
	public LispStruct analyze(final ArrayStruct<? extends LispStruct> input, final SemanticAnalyzer analyzer) {

		// TODO: unchecked here. meh
		final List<LispStruct> inputContents = (List<LispStruct>) input.getContents();

		for (int i = 0; i < inputContents.size(); i++) {
			final LispStruct currentElement = inputContents.get(i);
			final LispStruct analyzedElement = analyzer.analyzeForm(currentElement);
			inputContents.set(i, analyzedElement);
		}

		return input;
	}
}
