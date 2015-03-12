package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.SetqStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SetqExpander extends MacroFunctionExpander<SetqStruct> {

	private static final long serialVersionUID = 5324580926862048137L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.SETQ.setMacroFunctionExpander(this);
	}

	@Override
	public SetqStruct expand(final ListStruct form, final AnalysisBuilder analysisBuilder) {

		final List<LispStruct> forms = form.getRest().getAsJavaList();

		final int numberOfForms = forms.size();
		if ((numberOfForms % 2) != 0) {
			throw new ProgramErrorException("SETQ: Odd number of arguments received: " + form + ". Expected an even number of arguments.");
		}

		final List<SetqStruct.SetqPair> setqPairs = new ArrayList<>(numberOfForms / 2);

		for (int index = 0; index < forms.size(); index += 2) {

			final LispStruct var = forms.get(index);
			if (!(var instanceof SymbolStruct)) {
				throw new ProgramErrorException("SETQ: Variable must be of type SymbolStruct. Got: " + var);
			}
			final SymbolStruct<?> varSymbol = (SymbolStruct<?>) var;

			final LispStruct setqForm = forms.get(index + 1);
			final LispStruct setqFormAnalyzed = formAnalyzer.analyze(setqForm, analysisBuilder);

			final SetqStruct.SetqPair setqPair = new SetqStruct.SetqPair(varSymbol, setqFormAnalyzed);
			setqPairs.add(setqPair);
		}

		return new SetqStruct(setqPairs);
	}
}
