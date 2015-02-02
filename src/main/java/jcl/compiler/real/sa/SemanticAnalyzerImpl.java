package jcl.compiler.real.sa;

import jcl.LispStruct;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class SemanticAnalyzerImpl implements SemanticAnalyzer {

	private static final long serialVersionUID = -1291208288043954547L;

	private static final Logger LOGGER = LoggerFactory.getLogger(SemanticAnalyzerImpl.class);

	@Autowired
	private ListStructAnalyzer listStructAnalyzer;

	@Autowired
	private LexicalSymbolStructAnalyzer lexicalSymbolStructAnalyzer;

	@Override
	public LispStruct analyzeForm(final LispStruct form, final AnalysisBuilder analysisBuilder) {

		LispStruct analyzedForm = form;
		if (form instanceof ListStruct) {
			analyzedForm = listStructAnalyzer.analyze(this, (ListStruct) form, analysisBuilder);
		} else if (form instanceof SymbolStruct) {
			final SymbolStruct<?> symbolForm = (SymbolStruct<?>) form;
			analyzedForm = lexicalSymbolStructAnalyzer.analyzeSymbol(symbolForm, analysisBuilder);
		} else {
			LOGGER.warn("Unsupported Object Type sent through Analyzer: {}", form);
		}
		return analyzedForm;
	}
}
