package jcl.compiler.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;

import jcl.LispStruct;
import jcl.compiler.environment.Environment;
import jcl.compiler.environment.ProgvEnvironment;
import jcl.compiler.functions.EvalFunction;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.sa.analyzer.LispFormValueValidator;
import jcl.compiler.struct.specialoperator.PrognStruct;
import jcl.compiler.struct.specialoperator.ProgvStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperatorStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ProgvExpander extends MacroFunctionExpander<ProgvStruct> {

	@Autowired
	private FormAnalyzer formAnalyzer;

	@Autowired
	private LispFormValueValidator validator;

	@Override
	public SymbolStruct getFunctionSymbol() {
		return SpecialOperatorStruct.PROGV;
	}

	@Override
	public ProgvStruct expand(final ListStruct form, final Environment environment) {
		validator.validateListFormSize(form, 3, "PROGV");

		final ListStruct formRest = form.getRest();

		final LispStruct second = formRest.getCar();
		final ListStruct quotedVars = ListStruct.buildProperList(SpecialOperatorStruct.QUOTE, second);
		final ListStruct evalVars = ListStruct.buildProperList(EvalFunction.EVAL, quotedVars);
		final LispStruct analyzedEvalVars = formAnalyzer.analyze(evalVars, environment);

		final ListStruct formRestRest = formRest.getRest();

		final LispStruct third = formRestRest.getCar();
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
