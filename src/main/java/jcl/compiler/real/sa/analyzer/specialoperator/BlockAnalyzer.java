package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.BlockStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

@Component
public class BlockAnalyzer extends MacroFunctionExpander implements SpecialOperatorAnalyzer {

	private static final long serialVersionUID = -5185467468586381117L;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.BLOCK.setMacroFunctionExpander(this);
	}

	@Override
	public LispStruct expand(final ListStruct form, final AnalysisBuilder analysisBuilder) {
		return analyze(form, analysisBuilder);
	}

	@Override
	public BlockStruct analyze(final ListStruct input, final AnalysisBuilder analysisBuilder) {

		final int inputSize = input.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("BLOCK: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final ListStruct inputRest = input.getRest();

		final LispStruct second = inputRest.getFirst();
		if (!(second instanceof SymbolStruct)) {
			throw new ProgramErrorException("BLOCK: Label must be of type SymbolStruct. Got: " + second);
		}

		final SymbolStruct<?> name = (SymbolStruct<?>) second;
		analysisBuilder.getBlockStack().push(name);

		try {
			final List<LispStruct> forms = inputRest.getRest().getAsJavaList();

			final SemanticAnalyzer analyzer = analysisBuilder.getAnalyzer();

			final List<LispStruct> analyzedForms =
					forms.stream()
					     .map(e -> analyzer.analyzeForm(e, analysisBuilder))
					     .collect(Collectors.toList());

			return new BlockStruct(name, analyzedForms);
		} finally {
			analysisBuilder.getBlockStack().pop();
		}
	}
}
