package jcl.compiler.real.sa.analyzer.specialoperator;

import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.sa.FormAnalyzer;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.specialoperator.MultipleValueCallStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ListStruct;
import jcl.symbols.SpecialOperator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MultipleValueCallExpander extends MacroFunctionExpander<MultipleValueCallStruct> {

	private static final long serialVersionUID = -8350781874747372684L;

	@Autowired
	private FormAnalyzer formAnalyzer;

	/**
	 * Initializes the multiple-value-call macro function and adds it to the special operator 'multiple-value-call'.
	 */
	@PostConstruct
	private void init() {
		SpecialOperator.MULTIPLE_VALUE_CALL.setMacroFunctionExpander(this);
	}

	@Override
	public MultipleValueCallStruct expand(final ListStruct form, final Environment environment) {

		final int formSize = form.size();
		if (formSize < 2) {
			throw new ProgramErrorException("MULTIPLE-VALUE-CALL: Incorrect number of arguments: " + formSize + ". Expected at least 2 arguments.");
		}

		final ListStruct formRest = form.getRest();

		final LispStruct functionForm = formRest.getFirst();
		final LispStruct functionFormAnalyzed = formAnalyzer.analyze(functionForm, environment);

		final ListStruct formRestRest = formRest.getRest();

		final List<LispStruct> forms = formRestRest.getAsJavaList();
		final List<LispStruct> analyzedForms =
				forms.stream()
				     .map(e -> formAnalyzer.analyze(e, environment))
				     .collect(Collectors.toList());

		return new MultipleValueCallStruct(functionFormAnalyzed, analyzedForms);
	}
}
