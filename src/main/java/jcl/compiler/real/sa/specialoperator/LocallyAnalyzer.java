package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.special.DeclareAnalyzer;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class LocallyAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final LocallyAnalyzer INSTANCE = new LocallyAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input) {

		final ListStruct body = input.getRest();
		final List<LispStruct> bodyJavaList = body.getAsJavaList();
		final LocallyBodyResult locallyBodyResult = separateAndAnalyzeLocallyBody(bodyJavaList);

		final List<LispStruct> locallyResultList = new ArrayList<>();
		locallyResultList.add(SpecialOperator.LOCALLY);
		locallyResultList.addAll(locallyBodyResult.getDeclarations());
		locallyResultList.addAll(locallyBodyResult.getForms());

		return ListStruct.buildProperList(locallyResultList);
	}

	private static class LocallyBodyResult {

		private final List<LispStruct> declarations;
		private final List<LispStruct> forms;

		private LocallyBodyResult(final List<LispStruct> declarations, final List<LispStruct> forms) {
			this.declarations = declarations;
			this.forms = forms;
		}

		public List<LispStruct> getDeclarations() {
			return declarations;
		}

		public List<LispStruct> getForms() {
			return forms;
		}
	}

	private LocallyBodyResult separateAndAnalyzeLocallyBody(final List<LispStruct> locallyBody) {

		final List<LispStruct> declarations = new ArrayList<>();
		final List<LispStruct> forms = new ArrayList<>();

		final Iterator<LispStruct> iterator = locallyBody.iterator();
		if (iterator.hasNext()) {

			LispStruct next = iterator.next();
			while (iterator.hasNext() && (next instanceof ListStruct) && ((ListStruct) next).getFirst().equals(SpecialOperator.DECLARE)) {
				final LispStruct analyzedDeclaration = DeclareAnalyzer.INSTANCE.analyze(next);
				declarations.add(analyzedDeclaration);
				next = iterator.next();
			}

			while (iterator.hasNext()) {
				final LispStruct analyzedForm = SemanticAnalyzer.saMainLoop(next);
				forms.add(analyzedForm);
				next = iterator.next();
			}
		}

		return new LocallyBodyResult(declarations, forms);
	}
}
