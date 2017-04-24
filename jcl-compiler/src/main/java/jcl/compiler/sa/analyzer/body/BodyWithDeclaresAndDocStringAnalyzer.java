package jcl.compiler.sa.analyzer.body;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.StringStruct;
import jcl.lang.internal.SpecialOperatorStructImpl;
import org.springframework.stereotype.Component;

@Component
public class BodyWithDeclaresAndDocStringAnalyzer {

	public BodyProcessingResult analyze(final List<LispStruct> input) {

		final List<LispStruct> declares = new ArrayList<>();
		StringStruct docString = null;
		final List<LispStruct> bodyForms = new ArrayList<>();

		final Iterator<LispStruct> iterator = input.iterator();

		if (iterator.hasNext()) {
			LispStruct next = iterator.next();

			declares.add(SpecialOperatorStructImpl.DECLARE);

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

			if ((next instanceof StringStruct) && iterator.hasNext()) {
				docString = (StringStruct) next; // No need to analyze this
				next = iterator.next();
			}

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

		return new BodyProcessingResult(declares, docString, bodyForms);
	}

	private boolean isDeclaration(final LispStruct next) {
		return (next instanceof ListStruct) && ((ListStruct) next).getCar().eq(SpecialOperatorStructImpl.DECLARE);
	}
}
