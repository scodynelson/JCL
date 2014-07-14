package jcl.compiler.real.sa.specialoperator.special;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.compiler.real.sa.SymbolStructAnalyzer;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.Declaration;
import jcl.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

public class DeclareAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final DeclareAnalyzer INSTANCE = new DeclareAnalyzer();

	private static final Logger LOGGER = LoggerFactory.getLogger(DeclareAnalyzer.class);

	@Override
	public LispStruct analyze(final ListStruct input) {
		// there may not be any declarations
		if (input.getRest().equals(NullStruct.INSTANCE)) {
			return NullStruct.INSTANCE;
		}

		final List<LispStruct> javaRestList = input.getRest().getAsJavaList();
		for (final LispStruct nextDecl : javaRestList) {
			final ListStruct declarationSpec = (ListStruct) nextDecl;
			final Object declIdentifier = declarationSpec.getFirst();
			final ListStruct declList = declarationSpec.getRest();

			// now come the various cases
			if (declIdentifier.equals(Declaration.LISP_NAME)) {
				saLispNameDeclaration(declList);
			} else if (declIdentifier.equals(Declaration.JAVA_CLASS_NAME)) {
				saJavaNameDeclaration(declList);
			} else if (declIdentifier.equals(Declaration.NO_GENERATE_ANALYZER)) {
				saNoGenerateAnalyzerDeclaration(declList);
			} else if (declIdentifier.equals(Declaration.PARSED_LAMBDA_LIST)) {
				saParsedLambdaListDeclaration(declList);
			} else if (declIdentifier.equals(Declaration.DOCUMENTATION)) {
				saDocumentationDeclaration(declList);
			} else if (declIdentifier.equals(Declaration.SOURCE_FILE)) {
				saSourceFileDeclaration(declList);
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
				saSpecialDeclaration(declList);
			} else if (declIdentifier.equals(Declaration.TYPE)) {
				//we don't do anything here yet
			} else if (declIdentifier.equals(NullStruct.INSTANCE)) {
				//drop it on the floor
			} else {
				LOGGER.warn("DECLARE: unknown specifier: {}", declIdentifier);
			}
		}
		return input;
	}

	/**
	 * Handles the declaration of the Lisp name (SymbolStruct) of the function
	 */
	private static void saLispNameDeclaration(final ListStruct declarationList) {
	}

	/**
	 * Handles the declaration of the name of the Java class (SymbolStruct) of the function
	 */
	private static void saJavaNameDeclaration(final ListStruct declarationList) {
	}

	/**
	 * Handles the declaration of the name of the Java class (SymbolStruct) of the function
	 */
	private static void saNoGenerateAnalyzerDeclaration(final ListStruct declarationList) {
	}

	/**
	 * Handles the declaration of the parsed lambda list of the function
	 */
	private static void saParsedLambdaListDeclaration(final ListStruct declarationList) {
	}

	/**
	 * Handles the declaration of the name of the Java class (SymbolStruct) of the function
	 */
	private static void saDocumentationDeclaration(final ListStruct declarationList) {
	}

	private static void saSourceFileDeclaration(final ListStruct declarationList) {
	}

	/**
	 * handle the components of a Special declaration
	 */
	private static void saSpecialDeclaration(final ListStruct declarations) {
		final List<LispStruct> declaractionsJavaList = declarations.getAsJavaList();
		SymbolStruct sym = null;
		// Special declaration can apply to multiple SymbolStructs
		for (final LispStruct nextDecl : declaractionsJavaList) {
			try {
				sym = (SymbolStruct) nextDecl;
				SymbolStructAnalyzer.INSTANCE.analyze(sym);
			} catch (final ClassCastException ccExcption) {
				LOGGER.error("DECLARE: a non-SymbolStruct entity cannot be made SPECIAL: {}", sym);
			}
		}
	}
}
