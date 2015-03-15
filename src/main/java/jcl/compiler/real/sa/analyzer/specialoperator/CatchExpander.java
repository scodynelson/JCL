package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.CatchStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CatchExpander extends MacroFunctionExpander<CatchStruct> {

	private static final long serialVersionUID = -4421664278117234704L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the block macro function and adds it to the special operator 'block'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.CATCH.setMacroFunctionExpander(this);
	}

	@Override
	public CatchStruct expand(final ListStruct form, final Environment environment) {

		final int inputSize = form.size();
		if (inputSize < 2) {
			throw new ProgramErrorException("CATCH: Incorrect number of arguments: " + inputSize + ". Expected at least 2 arguments.");
		}

		final ListStruct inputRest = form.getRest();

		final LispStruct catchTag = inputRest.getFirst();
		final LispStruct catchTagAnalyzed = formAnalyzer.analyze(catchTag, environment);

		final List<LispStruct> forms = inputRest.getRest().getAsJavaList();

		final List<LispStruct> analyzedForms =
				forms.stream()
				     .map(e -> formAnalyzer.analyze(e, environment))
				     .collect(Collectors.toList());

		return new CatchStruct(catchTagAnalyzed, analyzedForms);
	}
}
