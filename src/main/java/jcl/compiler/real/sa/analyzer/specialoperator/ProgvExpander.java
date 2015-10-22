package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.ProgvEnvironment;
import jcl.compiler.real.functions.EvalFunction;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.ProgvStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ProgvExpander extends MacroFunctionExpander<ProgvStruct> {

	private static final long serialVersionUID = 2755221428467421207L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the progv macro function and adds it to the special operator 'progv'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperatorStruct.PROGV.setMacroFunctionExpander(this);
	}

	@Override
	public ProgvStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 3) {
			throw new ProgramErrorException("PROGV: Incorrect number of arguments: " + formSize + ". Expected at least 3 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getFirst();
		final ListStruct quotedVars = ListStruct.buildProperList(SpecialOperatorStruct.QUOTE, second);
		final ListStruct evalVars = ListStruct.buildProperList(EvalFunction.EVAL, quotedVars);
		final LispStruct analyzedEvalVars = formAnalyzer.analyze(evalVars, environment);

		final ListStruct formRestRest = formRest.getRest();

		final LispStruct third = formRestRest.getFirst();
		final ListStruct quotedVals = ListStruct.buildProperList(SpecialOperatorStruct.QUOTE, third);
		final ListStruct evalVals = ListStruct.buildProperList(EvalFunction.EVAL, quotedVals);
		final LispStruct analyzedEvalVals = formAnalyzer.analyze(evalVals, environment);

		// Handle Progn Environment processing
		final ProgvEnvironment progvEnvironment = new ProgvEnvironment(environment);

		final ListStruct formRestRestRest = formRestRest.getRest();

		final List<LispStruct> bodyForms = formRestRestRest.getAsJavaList();
		final List<LispStruct> analyzedBodyForms =
				bodyForms.stream()
				         .map(e -> formAnalyzer.analyze(e, environment))
				         .collect(Collectors.toList());

		return new ProgvStruct(analyzedEvalVars, analyzedEvalVals, new PrognStruct(analyzedBodyForms), progvEnvironment);
	}
}
