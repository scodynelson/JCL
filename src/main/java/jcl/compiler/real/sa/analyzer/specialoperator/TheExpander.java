package jcl.compiler.real.sa.analyzer.specialoperator;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.TheStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class TheExpander extends MacroFunctionExpander {

	private static final long serialVersionUID = 6723289642694216454L;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.THE.setMacroFunctionExpander(this);
	}

	@Override
	public TheStruct expand(final ListStruct form, final AnalysisBuilder analysisBuilder) {

		final int inputSize = form.size();
		if (inputSize != 3) {
			throw new ProgramErrorException("THE: Incorrect number of arguments: " + inputSize + ". Expected 3 arguments.");
		}

		final ListStruct inputRest = form.getRest();

		final LispStruct valueType = inputRest.getFirst();
		if (!(valueType instanceof SymbolStruct) && !(valueType instanceof ListStruct)) {
			throw new ProgramErrorException("THE: Type specifier must be of type SymbolStruct or ListStruct. Got: " + valueType);
		}
		// TODO: do we actually want to somehow factor in the 'TypeSpecifier' produced by the second value?

		final ListStruct inputRestRest = inputRest.getRest();

		final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

		final LispStruct theForm = inputRestRest.getFirst();
		final LispStruct theFormAnalyzed = analyzer.analyzeForm(theForm, analysisBuilder);

		return new TheStruct(null, theFormAnalyzed);
	}
}
