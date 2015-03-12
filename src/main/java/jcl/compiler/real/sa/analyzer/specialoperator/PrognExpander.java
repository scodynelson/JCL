package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class PrognExpander extends MacroFunctionExpander {

	private static final long serialVersionUID = -2851059577992887882L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.PROGN.setMacroFunctionExpander(this);
	}

	@Override
	public PrognStruct expand(final ListStruct form, final AnalysisBuilder analysisBuilder) {

		final List<LispStruct> forms = form.getRest().getAsJavaList();

		final List<LispStruct> analyzedForms =
				forms.stream()
				     .map(e -> formAnalyzer.analyze(e, analysisBuilder))
				     .collect(Collectors.toList());

		return new PrognStruct(analyzedForms);
	}
}
