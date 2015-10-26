package jcl.compiler.sa.analyzer.body;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import org.springframework.stereotype.Component;

@Component
public class BodyWithDeclaresAndDocStringAnalyzer implements Serializable {

	private static final long serialVersionUID = 3031836027613475526L;

	public BodyProcessingResult analyze(final List<LispStruct> input) {

		final List<LispStruct> declares = new ArrayList<>();
		StringStruct docString = null;
		final List<LispStruct> bodyForms = new ArrayList<>();

		final Iterator<LispStruct> iterator = input.iterator();

		if (iterator.hasNext()) {
			LispStruct next = iterator.next();

			declares.add(SpecialOperatorStruct.DECLARE);

			while (isDeclaration(next)) {
				final ListStruct declareStatement = (ListStruct) next;
				final List<LispStruct> declarations = declareStatement.getRest().getAsJavaList();

				declares.addAll(declarations);

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
				final List<LispStruct> declarations = declareStatement.getRest().getAsJavaList();

				declares.addAll(declarations);

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
		return (next instanceof ListStruct) && ((ListStruct) next).getFirst().equals(SpecialOperatorStruct.DECLARE);
	}
}
