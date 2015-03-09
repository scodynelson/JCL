package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.MultipleValueCallStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.stereotype.Component;

@Component
public class MultipleValueCallAnalyzer extends MacroFunctionExpander implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -8350781874747372684L;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.MULTIPLE_VALUE_CALL.setMacroFunctionExpander(this);
	}

	@Override
	public LispStruct expand(final ListStruct form, final AnalysisBuilder analysisBuilder) {
		return analyze(form, analysisBuilder);
	}

	@Override
	public MultipleValueCallStruct analyze(final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final int inputSize = input.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("MULTIPLE-VALUE-CALL: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final ListStruct inputRest = input.getRest();

		final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

		final LispStruct functionForm = inputRest.getFirst();
		final LispStruct functionFormAnalyzed = analyzer.analyzeForm(functionForm, analysisBuilder);

		final List<LispStruct> forms = inputRest.getRest().getAsJavaList();
		final List<LispStruct> analyzedForms =
				forms.stream()
				     .map(e -> analyzer.analyzeForm(e, analysisBuilder))
				     .collect(Collectors.toList());

		return new MultipleValueCallStruct(functionFormAnalyzed, analyzedForms);
	}
}
