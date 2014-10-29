package jcl.compiler.real.sa.specialoperator.special;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.SymbolStructAnalyzer;
import jcl.structs.conditions.exceptions.ProgramErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.symbols.Declaration;
import jcl.structs.symbols.SymbolStruct;

import java.util.List;

public class DeclareAnalyzer implements Analyzer<LispStruct, LispStruct> {

	public static final DeclareAnalyzer INSTANCE = new DeclareAnalyzer();

	@Override
	public LispStruct analyze(final LispStruct input, final SemanticAnalyzer semanticAnalyzer) {

		final List<LispStruct> javaRestList = ((ListStruct) input).getRest().getAsJavaList();
		for (final LispStruct nextDecl : javaRestList) {

			final ListStruct declarationSpec = (ListStruct) nextDecl;
			final Object declIdentifier = declarationSpec.getFirst();
			final ListStruct declList = declarationSpec.getRest();

			// now come the various cases
			if (declIdentifier.equals(Declaration.LISP_NAME)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.JAVA_CLASS_NAME)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.NO_GENERATE_ANALYZER)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.PARSED_LAMBDA_LIST)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.DOCUMENTATION)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.SOURCE_FILE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.DECLARATION)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.DYNAMIC_EXTENT)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.FTYPE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.IGNORABLE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.IGNORE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.INLINE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.NOTINLINE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.OPTIMIZE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(Declaration.SPECIAL)) {
				saSpecialDeclaration(semanticAnalyzer, declList);
			} else if (declIdentifier.equals(Declaration.TYPE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(NullStruct.INSTANCE)) {
				//drop it on the floor
			} else {
				throw new ProgramErrorException("DECLARE: unknown specifier: " + declIdentifier);
			}
		}
		return input;
	}

	private static void saSpecialDeclaration(final SemanticAnalyzer semanticAnalyzer, final ListStruct declarations) {
		final List<LispStruct> declarationsAsJavaList = declarations.getAsJavaList();

		// Special declaration can apply to multiple SymbolStructs
		for (final LispStruct nextDecl : declarationsAsJavaList) {
			if (!(nextDecl instanceof SymbolStruct)) {
				throw new ProgramErrorException("DECLARE: a non-SymbolStruct entity cannot be made SPECIAL: " + nextDecl);
			}

			final SymbolStruct<?> sym = (SymbolStruct) nextDecl;
			SymbolStructAnalyzer.INSTANCE.analyze(sym, semanticAnalyzer);
		}
	}
}
