package jcl.compiler.real.sa.analyzer.specialoperator;

import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.sa.AnalysisBuilder;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.real.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.ReturnFromStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ReturnFromExpander extends MacroFunctionExpander<ReturnFromStruct> {

	private static final long serialVersionUID = 3328790948675693554L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.RETURN_FROM.setMacroFunctionExpander(this);
	}

	@Override
	public ReturnFromStruct expand(final ListStruct form, final AnalysisBuilder analysisBuilder) {

		final int inputSize = form.size();
		if ((inputSize < 2) || (inputSize > 3)) {
			throw new ProgramErrorException("RETURN-FROM: Incorrect number of arguments: " + inputSize + ". Expected either 2 or 3 arguments.");
		}

		final ListStruct inputRest = form.getRest();

		final LispStruct second = inputRest.getFirst();
		if (!(second instanceof SymbolStruct)) {
			throw new ProgramErrorException("RETURN-FROM: Name must be of type SymbolStruct. Got: " + second);
		}

		final SymbolStruct<?> name = (SymbolStruct<?>) second;

		if (analysisBuilder.getBlockStack().search(name) == -1) {
			throw new ProgramErrorException("RETURN-FROM: No BLOCK with name " + second + " is visible.");
		}

		if (inputSize == 3) {
			final ListStruct inputRestRest = inputRest.getRest();

			final LispStruct result = inputRestRest.getFirst();
			final LispStruct analyzedResult = formAnalyzer.analyze(result, analysisBuilder);
			return new ReturnFromStruct(name, analyzedResult);
		} else {
			return new ReturnFromStruct(name);
		}
	}
}