package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.specialoperator.special.DeclareAnalyzer;
import jcl.structs.arrays.StringStruct;
import jcl.structs.lists.ListStruct;
import jcl.structs.symbols.SpecialOperator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public final class BodyProcessingUtil {

	public static class BodyProcessingResult {

		private final List<LispStruct> declarations;
		private final StringStruct docString;
		private final List<LispStruct> bodyForms;

		private BodyProcessingResult(final List<LispStruct> declarations, final StringStruct docString,
		                             final List<LispStruct> bodyForms) {
			this.declarations = declarations;
			this.docString = docString;
			this.bodyForms = bodyForms;
		}

		public List<LispStruct> getDeclarations() {
			return declarations;
		}

		public StringStruct getDocString() {
			return docString;
		}

		public List<LispStruct> getBodyForms() {
			return bodyForms;
		}
	}

	public static BodyProcessingResult processBody(final ListStruct body) {
		final List<LispStruct> bodyJavaList = body.getAsJavaList();

		final List<LispStruct> declarations = new ArrayList<>();
		final List<LispStruct> bodyForms = new ArrayList<>();

		final Iterator<LispStruct> iterator = bodyJavaList.iterator();
		if (iterator.hasNext()) {

			LispStruct next = iterator.next();
			while (iterator.hasNext() && (next instanceof ListStruct) && ((ListStruct) next).getFirst().equals(SpecialOperator.DECLARE)) {
				final LispStruct analyzedDeclaration = DeclareAnalyzer.INSTANCE.analyze(next);
				declarations.add(analyzedDeclaration);
				next = iterator.next();
			}

			while (iterator.hasNext()) {
				final LispStruct analyzedForm = SemanticAnalyzer.saMainLoop(next);
				bodyForms.add(analyzedForm);
				next = iterator.next();
			}
		}

		return new BodyProcessingResult(declarations, null, bodyForms);
	}

	public static BodyProcessingResult processBodyWithDoc(final ListStruct body) {
		final List<LispStruct> bodyJavaList = body.getAsJavaList();

		final List<LispStruct> declarations = new ArrayList<>();
		StringStruct docString = null;
		final List<LispStruct> bodyForms = new ArrayList<>();

		final Iterator<LispStruct> iterator = bodyJavaList.iterator();
		if (iterator.hasNext()) {

			LispStruct next = iterator.next();
			while (iterator.hasNext() && (next instanceof ListStruct) && ((ListStruct) next).getFirst().equals(SpecialOperator.DECLARE)) {
				final LispStruct analyzedDeclaration = DeclareAnalyzer.INSTANCE.analyze(next); // TODO: really analyze these???
				declarations.add(analyzedDeclaration);
				next = iterator.next();
			}

			if ((next instanceof StringStruct) && iterator.hasNext()) {
				docString = (StringStruct) next; // No need to analyze this
				next = iterator.next();
			}

			while (iterator.hasNext()) {
				final LispStruct analyzedForm = SemanticAnalyzer.saMainLoop(next);
				bodyForms.add(analyzedForm);
				next = iterator.next();
			}
		}

		return new BodyProcessingResult(declarations, docString, bodyForms);
	}
}
