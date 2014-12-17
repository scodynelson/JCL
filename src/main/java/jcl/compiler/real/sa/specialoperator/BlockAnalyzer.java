package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.element.BlockElement;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class BlockAnalyzer implements SpecialOperatorAnalyzer {

	@Override
	public LispStruct analyze(final SemanticAnalyzer analyzer, final ListStruct input, final AnalysisBuilder analysisBuilder) {

		if (input.size() < 2) {
			throw new ProgramErrorException("BLOCK: Incorrect number of arguments: " + input.size() + ". Expected at least 2 arguments.");
		}

		final LispStruct second = input.getRest().getFirst();
		if (!(second instanceof SymbolStruct)) {
			throw new ProgramErrorException("BLOCK: Label must be of type SymbolStruct. Got: " + second);
		}

		final SymbolStruct<?> name = (SymbolStruct) second;
		analysisBuilder.getBlockStack().push(name);

		try {
			final ListStruct forms = input.getRest().getRest();

			final List<LispStruct> formsJavaList = forms.getAsJavaList();
			final List<LispStruct> analyzedForms =
					formsJavaList.stream()
					             .map(e -> analyzer.analyzeForm(e, analysisBuilder))
					             .collect(Collectors.toList());

			return new BlockElement(name, analyzedForms);
		} finally {
			analysisBuilder.getBlockStack().pop();
		}
	}
}
