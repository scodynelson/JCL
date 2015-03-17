package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.MultipleValueProg1Struct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MultipleValueProg1Expander extends MacroFunctionExpander<MultipleValueProg1Struct> {

	private static final long serialVersionUID = 1791554561862006171L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the multiple-value-prog1 macro function and adds it to the special operator 'multiple-value-prog1'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.MULTIPLE_VALUE_PROG1.setMacroFunctionExpander(this);
	}

	@Override
	public MultipleValueProg1Struct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 2) {
			throw new ProgramErrorException("MULTIPLE-VALUE-PROG1: Incorrect number of arguments: " + formSize + ". Expected at least 2 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct firstForm = formRest.getFirst();
		final LispStruct firstFormAnalyzed = formAnalyzer.analyze(firstForm, environment);

		final ListStruct formRestRest = formRest.getRest();

		final List<LispStruct> forms = formRestRest.getAsJavaList();
		final List<LispStruct> analyzedForms =
				forms.stream()
				     .map(e -> formAnalyzer.analyze(e, environment))
				     .collect(Collectors.toList());

		return new MultipleValueProg1Struct(firstFormAnalyzed, analyzedForms);
	}
}
