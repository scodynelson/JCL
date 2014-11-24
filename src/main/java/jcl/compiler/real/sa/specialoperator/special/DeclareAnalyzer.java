package jcl.compiler.real.sa.specialoperator.special;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.SymbolStructAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.Declaration;
import jcl.symbols.SymbolStruct;

import java.util.List;

public class DeclareAnalyzer implements Analyzer<ListStruct, ListStruct> {

	public static final DeclareAnalyzer INSTANCE = new DeclareAnalyzer();

	@Override
	public ListStruct analyze(final ListStruct input, final SemanticAnalyzer analyzer) {

		final ListStruct declSpecs = input.getRest();

		final List<LispStruct> declSpecsJavaList = declSpecs.getAsJavaList();
		for (final LispStruct declSpec : declSpecsJavaList) {

			if (!(declSpec instanceof ListStruct)) {
				throw new ProgramErrorException("DECLARE: Declaration specifier must be of type ListStruct. Got: " + declSpec);
			}

			final ListStruct declSpecList = (ListStruct) declSpec;
			final Object declIdentifier = declSpecList.getFirst();
			final ListStruct declSpecBody = declSpecList.getRest();

			// now come the various cases
			if (declIdentifier.equals(Declaration.DECLARATION)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.DYNAMIC_EXTENT)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.FTYPE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.IGNORABLE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.IGNORE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.INLINE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.NOTINLINE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.OPTIMIZE)) {
				//TODO: we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.SPECIAL)) {
				saSpecialDeclaration(declSpecBody, analyzer);
			} else if (declIdentifier.equals(Declaration.TYPE)) {
				//we don't do anything here yet
			} else {
				throw new ProgramErrorException("DECLARE: Declaration specifier not supported: " + declIdentifier);
			}
		}

		return NullStruct.INSTANCE;
	}

	private static void saSpecialDeclaration(final ListStruct declSpecBody, final SemanticAnalyzer analyzer) {
		final List<LispStruct> declSpecBodyJavaList = declSpecBody.getAsJavaList();

		// Special declaration can apply to multiple SymbolStructs
		for (final LispStruct declSpecBodyElement : declSpecBodyJavaList) {
			if (!(declSpecBodyElement instanceof SymbolStruct)) {
				throw new ProgramErrorException("DECLARE: a non-SymbolStruct entity cannot be made SPECIAL: " + declSpecBodyElement);
			}

			final SymbolStruct<?> sym = (SymbolStruct) declSpecBodyElement;
			SymbolStructAnalyzer.INSTANCE.analyze(sym, analyzer);
		}
	}
}
