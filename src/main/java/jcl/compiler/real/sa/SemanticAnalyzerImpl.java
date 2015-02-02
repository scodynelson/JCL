package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class SemanticAnalyzerImpl implements SemanticAnalyzer {

	private static final long serialVersionUID = -1291208288043954547L;

	@Autowired
	private ListStructAnalyzer listStructAnalyzer;

	@Autowired
	private SymbolStructAnalyzer symbolStructAnalyzer;

	@Autowired
	private ArrayStructAnalyzer arrayStructAnalyzer;

	@Override
	public LispStruct analyzeForm(final LispStruct form, final AnalysisBuilder analysisBuilder) {

		LispStruct analyzedForm = form;
		if (form instanceof ListStruct) {
			analyzedForm = listStructAnalyzer.analyze(this, (ListStruct) form, analysisBuilder);
		} else if (form instanceof SymbolStruct) {
			final SymbolStruct<?> symbolForm = (SymbolStruct<?>) form;
			analyzedForm = symbolStructAnalyzer.analyze(symbolForm, analysisBuilder, symbolForm.isSpecial());
		} else if (form instanceof ArrayStruct) {
			analyzedForm = arrayStructAnalyzer.analyze(this, (ArrayStruct<?>) form, analysisBuilder);
		}
		return analyzedForm;
	}
}
