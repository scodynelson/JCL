package jcl.compiler.sa.analyzer.body;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.internal.SpecialOperatorStructImpl;

public class BodyWithDeclaresAnalyzer {

	public static BodyProcessingResult analyze(final List<LispStruct> input) {

		final List<LispStruct> declares = new ArrayList<>();
		final List<LispStruct> bodyForms = new ArrayList<>();

		final Iterator<LispStruct> iterator = input.iterator();

		declares.add(SpecialOperatorStructImpl.DECLARE);

		if (iterator.hasNext()) {
			LispStruct next = iterator.next();

			while (isDeclaration(next)) {
				final ListStruct declareStatement = (ListStruct) next;
				final Iterator<LispStruct> declareIterator = declareStatement.iterator();
				declareIterator.next(); // DECLARE SYMBOL
				declareIterator.forEachRemaining(declares::add);

				if (iterator.hasNext()) {
					next = iterator.next();
				} else {
					next = null;
					break;
				}
			}

			if (next != null) {
				bodyForms.add(next);
			}
			while (iterator.hasNext()) {
				next = iterator.next();
				bodyForms.add(next);
			}
		}

		return new BodyProcessingResult(declares, null, bodyForms);
	}

	private static boolean isDeclaration(final LispStruct next) {
		return (next instanceof ListStruct) && ((ListStruct) next).car().eq(SpecialOperatorStructImpl.DECLARE);
	}
}
