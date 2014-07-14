package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.old.symbol.KeywordOld;
import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;

public class EvalWhenAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final EvalWhenAnalyzer INSTANCE = new EvalWhenAnalyzer();

	@Override
	public LispStruct analyze(final ListStruct input) {
		// this only has effect when the :execute situation is found
		// :compile-toplevel and :load-toplevel situations are ignored.
		// for any of the nested code to be compiled, the :execute
		// situation is in effect.

		final LispStruct second = input.getRest().getFirst();
		if (second instanceof ListStruct) {
			final ListStruct situationList = (ListStruct) second;

			if (situationList.getAsJavaList().contains(KeywordOld.Execute)) {
				final ListStruct formsList = input.getRest();
				return PrognAnalyzer.INSTANCE.analyze(formsList);
			} else {
				return NullStruct.INSTANCE;
			}
		}

		throw new RuntimeException("Improperly formed EVAL-WHEN: " + input);
	}
}
