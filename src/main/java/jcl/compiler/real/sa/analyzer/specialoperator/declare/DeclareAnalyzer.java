package jcl.compiler.real.sa.analyzer.specialoperator.declare;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.ListElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.compiler.real.element.specialoperator.declare.DeclareElement;
import jcl.compiler.real.element.specialoperator.declare.SpecialDeclarationElement;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.DynamicSymbolAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.sa.analyzer.specialoperator.SpecialOperatorAnalyzer;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.symbols.Declaration;
import jcl.symbols.SpecialOperator;
import jcl.system.EnhancedLinkedList;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.List;

@Component
public class DeclareAnalyzer extends MacroFunctionExpander implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -27949883247210201L;

	@Autowired
	private DynamicSymbolAnalyzer dynamicSymbolAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.DECLARE.setMacroFunctionExpander(this);
	}

	@Override
	public Element expand(final ConsElement form, final AnalysisBuilder analysisBuilder) {
		return analyze(form, analysisBuilder);
	}

	@Override
	public DeclareElement analyze(final ConsElement input, final AnalysisBuilder analysisBuilder) {

		final EnhancedLinkedList<SimpleElement> elements = input.getElements();
		final EnhancedLinkedList<SimpleElement> declSpecs = elements.getAllButFirst();

		final DeclareElement declareElement = new DeclareElement();

		for (final SimpleElement declSpec : declSpecs) {

			if (!(declSpec instanceof ListElement)) {
				throw new ProgramErrorException("DECLARE: Declaration specifier must be of type ListStruct. Got: " + declSpec);
			}

			final ListElement declSpecList = (ListElement) declSpec;
			final Object declIdentifier = null; //TODO: declSpecList.getFirst();
			final EnhancedLinkedList<SimpleElement> declSpecBody = null; //TODO: declSpecList.getRest();

			final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

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
				final List<SpecialDeclarationElement> sdes = saSpecialDeclaration(analyzer, analysisBuilder, declSpecBody);
				declareElement.getSpecialDeclarationElements().addAll(sdes);
			} else if (declIdentifier.equals(Declaration.TYPE)) {
				//we don't do anything here yet
			} else {
				throw new ProgramErrorException("DECLARE: Declaration specifier not supported: " + declIdentifier);
			}
		}

		return declareElement;
	}

	private List<SpecialDeclarationElement> saSpecialDeclaration(final SemanticAnalyzer analyzer, final AnalysisBuilder analysisBuilder,
	                                                             final EnhancedLinkedList<SimpleElement> declSpecBody) {

		final List<SpecialDeclarationElement> specialDeclarationElements = new ArrayList<>(declSpecBody.size());

		// Special declaration can apply to multiple SymbolStructs
		for (final SimpleElement declSpecBodyElement : declSpecBody) {
			if (!(declSpecBodyElement instanceof SymbolElement)) {
				throw new ProgramErrorException("DECLARE: a non-SymbolStruct entity cannot be made SPECIAL: " + declSpecBodyElement);
			}

			final SymbolElement sym = (SymbolElement) declSpecBodyElement;

			dynamicSymbolAnalyzer.analyze(sym, analysisBuilder);

			final SpecialDeclarationElement specialDeclarationElement = new SpecialDeclarationElement(sym);
			specialDeclarationElements.add(specialDeclarationElement);
		}

		return specialDeclarationElements;
	}
}
