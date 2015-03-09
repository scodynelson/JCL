package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.SetqStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class SetqAnalyzer extends MacroFunctionExpander implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = 5324580926862048137L;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.SETQ.setMacroFunctionExpander(this);
	}

	@Override
	public LispStruct expand(final ListStruct form, final AnalysisBuilder analysisBuilder) {
		return analyze(form, analysisBuilder);
	}

	@Override
	public SetqStruct analyze(final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final List<LispStruct> forms = input.getRest().getAsJavaList();

		final int numberOfForms = forms.size();
		if ((numberOfForms % 2) != 0) {
			throw new ProgramErrorException("SETQ: Odd number of arguments received: " + input + ". Expected an even number of arguments.");
		}

		final List<SetqStruct.SetqPair> setqPairs = new ArrayList<>(numberOfForms / 2);

		for (int index = 0; index < forms.size(); index += 2) {

			final LispStruct var = forms.get(index);
			if (!(var instanceof SymbolStruct)) {
				throw new ProgramErrorException("SETQ: Variable must be of type SymbolStruct. Got: " + var);
			}
			final SymbolStruct<?> varSymbol = (SymbolStruct<?>) var;

			final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

			final LispStruct form = forms.get(index + 1);
			final LispStruct formAnalyzed = analyzer.analyzeForm(form, analysisBuilder);

			final SetqStruct.SetqPair setqPair = new SetqStruct.SetqPair(varSymbol, formAnalyzed);
			setqPairs.add(setqPair);
		}

		return new SetqStruct(setqPairs);
	}
}
