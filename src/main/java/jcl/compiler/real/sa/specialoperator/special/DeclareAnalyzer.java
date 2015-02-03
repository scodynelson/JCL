package jcl.compiler.real.sa.specialoperator.special;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.DynamicSymbolStructAnalyzer;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.declaration.DeclareElement;
import jcl.compiler.real.sa.element.declaration.SpecialDeclarationElement;
import jcl.compiler.real.sa.specialoperator.SpecialOperatorAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.Declaration;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class DeclareAnalyzer implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -27949883247210201L;

	@Autowired
	private DynamicSymbolStructAnalyzer dynamicSymbolStructAnalyzer;

	@Override
	public DeclareElement analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final ListStruct declSpecs = input.getRest();

		final DeclareElement declareElement = new DeclareElement();

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
				final List<SpecialDeclarationElement> sdes = saSpecialDeclaration(declSpecBody, analysisBuilder);
				declareElement.getSpecialDeclarationElements().addAll(sdes);
			} else if (declIdentifier.equals(Declaration.TYPE)) {
				//we don't do anything here yet
			} else {
				throw new ProgramErrorException("DECLARE: Declaration specifier not supported: " + declIdentifier);
			}
		}

		return declareElement;
	}

	private List<SpecialDeclarationElement> saSpecialDeclaration(final ListStruct declSpecBody, final AnalysisBuilder analysisBuilder) {
		final List<LispStruct> declSpecBodyJavaList = declSpecBody.getAsJavaList();

		final List<SpecialDeclarationElement> specialDeclarationElements = new ArrayList<>(declSpecBodyJavaList.size());

		// Special declaration can apply to multiple SymbolStructs
		for (final LispStruct declSpecBodyElement : declSpecBodyJavaList) {
			if (!(declSpecBodyElement instanceof SymbolStruct)) {
				throw new ProgramErrorException("DECLARE: a non-SymbolStruct entity cannot be made SPECIAL: " + declSpecBodyElement);
			}

			final SymbolStruct<?> sym = (SymbolStruct) declSpecBodyElement;

			dynamicSymbolStructAnalyzer.analyzeSymbol(sym, analysisBuilder);

			final SpecialDeclarationElement specialDeclarationElement = new SpecialDeclarationElement(sym);
			specialDeclarationElements.add(specialDeclarationElement);
		}

		return specialDeclarationElements;
	}
}
